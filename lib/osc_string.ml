open Osc_result

module Input = struct
  type t = {
    data: string;
    mutable pos: int;
  }

  let current_char input = input.data.[input.pos]
end

exception Not_implemented

(* Strings are padding with 1-4 null characters to make the total
 * length a multiple of 4 bytes. *)
let string_padding_of_length length =
  4 - (length mod 4)

(* Blobs are padded with 0-3 null characters to make the total
 * length a multiple of 4 bytes. *)
let blob_padding_of_length length =
  match length mod 4 with
  | 0 -> 0
  | x -> 4 - x

let int32_chars = 4

module Decode = struct
  open Input

  let int32 input =
    let result = EndianString.BigEndian.get_int32 input.data input.pos in
    input.pos <- input.pos + int32_chars;
    result

  let float32 input =
    Int32.float_of_bits (int32 input)

  let string input =
    (* Look for the first null char after the position marker - this is the
     * start of the string's padding. *)
    let end_pos = Bytes.index_from input.data input.pos '\000' in
    let string_length = end_pos - input.pos in
    let padding_length = string_padding_of_length string_length in
    (* Read the string, then move the position marker past the string and its
     * padding. *)
    let result = Bytes.sub input.data input.pos string_length in
    input.pos <- input.pos + string_length + padding_length;
    result

  let blob input =
    (* Decode the blob length. *)
    let blob_length = Int32.to_int (int32 input) in
    let padding_length = blob_padding_of_length blob_length in
    (* Read the blob, then move the position marker past the blob and its
     * padding. *)
    let result = Bytes.sub input.data input.pos blob_length in
    input.pos <- input.pos + blob_length + padding_length;
    result

  let argument input = function
    | 'f' -> return (Osc.Float32 (float32 input))
    | 'i' -> return (Osc.Int32 (int32 input))
    | 's' -> return (Osc.String (string input))
    | 'b' -> return (Osc.Blob (blob input))
    | typetag -> fail (`Unsupported_typetag typetag)

  let arguments input =
    if current_char input <> ','
    then fail `Missing_typetag_string
    else begin
      (* Decode the typetag string. *)
      let typetag_string = string input in
      let typetag_count = (Bytes.length typetag_string) - 1 in
      (* Decode the arguments, moving along the typetag string to detect the
       * type we're trying to decode. Due to the ',' prefix in the typetag
       * string, the first typetag is the second character in the typetag
       * string. *)
      let rec decode typetag_position acc =
        if typetag_position > typetag_count
        then return acc
        else
          argument input typetag_string.[typetag_position]
          >>= (fun arg -> decode (typetag_position + 1) (arg :: acc))
      in
      decode 1 [] >|= List.rev
    end

  let timetag input =
    let seconds = int32 input in
    let fraction = int32 input in
    match seconds, fraction with
    | 0l, 1l -> Osc.Immediate
    | _ -> Osc.(Time {seconds; fraction})

  let packet input =
    match string input with
    | "#bundle" -> raise Not_implemented
    | address ->
      arguments input >>=
      (fun args -> return Osc.(Message {address = address; arguments = args}))
end

module Encode = struct
  let int32 output i =
    let tmp = Bytes.create int32_chars in
    EndianString.BigEndian.set_int32 tmp 0 i;
    Buffer.add_string output tmp

  let float32 output f =
    int32 output (Int32.bits_of_float f)

  let string output s =
    Buffer.add_string output s;
    let string_length = Bytes.length s in
    let padding_length = string_padding_of_length string_length in
    let padding = Bytes.make padding_length '\000' in
    Buffer.add_string output padding

  let blob output b =
    (* Encode the blob length as an int32. *)
    let blob_length = Bytes.length b in
    int32 output (Int32.of_int blob_length);
    (* Encode the blob itself, followed by a suitable amount of padding. *)
    Buffer.add_string output b;
    let padding_length = blob_padding_of_length blob_length in
    if padding_length > 0 then begin
      let padding = Bytes.make padding_length '\000' in
      Buffer.add_string output padding
    end

  let argument output = function
    | Osc.Float32 f -> float32 output f
    | Osc.Int32 i -> int32 output i
    | Osc.String s -> string output s
    | Osc.Blob b -> blob output b

  let arguments output args =
    let typetag_of_argument = function
      | Osc.Float32 _ -> 'f'
      | Osc.Int32 _ -> 'i'
      | Osc.String _ -> 's'
      | Osc.Blob _ -> 'b'
    in
    (* Encode the typetags as a string, prefixed with a comma. *)
    let typetag_string = Bytes.create ((List.length args) + 1) in
    Bytes.set typetag_string 0 ',';
    List.iteri
      (fun index arg ->
        Bytes.set typetag_string (index + 1) (typetag_of_argument arg))
      args;
    string output typetag_string;
    (* Encode the values of the arguments. *)
    let rec encode = function
      | [] -> ()
      | arg :: rest ->
          argument output arg;
          encode rest
    in
    encode args

  let timetag output =
    let open Osc in function
    | Immediate ->
      int32 output 0l;
      int32 output 1l;
    | Time {seconds; fraction} ->
      int32 output seconds;
      int32 output fraction

  let packet output = function
    | Osc.Bundle _ -> raise Not_implemented
    | Osc.Message msg ->
      string output msg.Osc.address;
      arguments output msg.Osc.arguments
end

let of_packet packet =
  let output = Buffer.create 64 in
  Encode.packet output packet;
  Buffer.contents output

let to_packet data =
  let input = Input.({data; pos = 0}) in
  Decode.packet input
