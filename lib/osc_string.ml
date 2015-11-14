open Rresult

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

  let decode_int32 input =
    let result = EndianString.BigEndian.get_int32 input.data input.pos in
    input.pos <- input.pos + int32_chars;
    result

  let decode_float32 input =
    Int32.float_of_bits (decode_int32 input)

  let decode_string input =
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

  let decode_blob input =
    (* Decode the blob length. *)
    let blob_length = Int32.to_int (decode_int32 input) in
    let padding_length = blob_padding_of_length blob_length in
    (* Read the blob, then move the position marker past the blob and its
     * padding. *)
    let result = Bytes.sub input.data input.pos blob_length in
    input.pos <- input.pos + blob_length + padding_length;
    result

  let decode_argument input = function
    | 'f' -> Ok (Osc.Float32 (decode_float32 input))
    | 'i' -> Ok (Osc.Int32 (decode_int32 input))
    | 's' -> Ok (Osc.String (decode_string input))
    | 'b' -> Ok (Osc.Blob (decode_blob input))
    | typetag -> Error (`Unsupported_typetag typetag)

  let decode_arguments input =
    if current_char input <> ','
    then Error `Missing_typetag_string
    else begin
      (* Decode the typetag string. *)
      let typetag_string = decode_string input in
      let typetag_count = (Bytes.length typetag_string) - 1 in
      (* Decode the arguments, moving along the typetag string to detect the
       * type we're trying to decode. Due to the ',' prefix in the typetag
       * string, the first typetag is the second character in the typetag
       * string. *)
      let rec decode typetag_position acc =
        if typetag_position > typetag_count
        then (Ok acc)
        else
          decode_argument input typetag_string.[typetag_position]
          >>= (fun arg -> decode (typetag_position + 1) (arg :: acc))
      in
      decode 1 [] >>| List.rev
    end

  let decode_timetag input =
    let seconds = decode_int32 input in
    let fraction = decode_int32 input in
    match seconds, fraction with
    | 0l, 1l -> Osc.Immediate
    | _ -> Osc.(Time {seconds; fraction})

  let decode_packet input =
    match decode_string input with
    | "#bundle" -> raise Not_implemented
    | address ->
      decode_arguments input >>=
      (fun args -> Ok (Osc.(Message {address = address; arguments = args})))
end

module Encode = struct
  let encode_int32 output i =
    let tmp = Bytes.create int32_chars in
    EndianString.BigEndian.set_int32 tmp 0 i;
    Buffer.add_string output tmp

  let encode_float32 output f =
    encode_int32 output (Int32.bits_of_float f)

  let encode_string output s =
    Buffer.add_string output s;
    let string_length = Bytes.length s in
    let padding_length = string_padding_of_length string_length in
    let padding = Bytes.make padding_length '\000' in
    Buffer.add_string output padding

  let encode_blob output b =
    (* Encode the blob length as an int32. *)
    let blob_length = Bytes.length b in
    encode_int32 output (Int32.of_int blob_length);
    (* Encode the blob itself, followed by a suitable amount of padding. *)
    Buffer.add_string output b;
    let padding_length = blob_padding_of_length blob_length in
    if padding_length > 0 then begin
      let padding = Bytes.make padding_length '\000' in
      Buffer.add_string output padding
    end

  let encode_argument output = function
    | Osc.Float32 f -> encode_float32 output f
    | Osc.Int32 i -> encode_int32 output i
    | Osc.String s -> encode_string output s
    | Osc.Blob b -> encode_blob output b

  let encode_arguments output args =
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
    encode_string output typetag_string;
    (* Encode the values of the arguments. *)
    let rec encode = function
      | [] -> ()
      | arg :: rest ->
          encode_argument output arg;
          encode rest
    in
    encode args

  let encode_timetag output =
    let open Osc in function
    | Immediate ->
      encode_int32 output 0l;
      encode_int32 output 1l;
    | Time {seconds; fraction} ->
      encode_int32 output seconds;
      encode_int32 output fraction

  let rec encode_bundle output {Osc.timetag; packets} =
    encode_timetag output timetag;
    List.iter
      (fun packet ->
        let sub_output = Buffer.create 20 in
        encode_packet sub_output packet;
        let packet_length = Buffer.length sub_output in
        encode_int32 output (Int32.of_int packet_length);
        Buffer.add_buffer output sub_output)
      packets

  and encode_packet output = function
    | Osc.Bundle bundle ->
      encode_string output "#bundle";
      encode_bundle output bundle
    | Osc.Message msg ->
      encode_string output msg.Osc.address;
      encode_arguments output msg.Osc.arguments
end

let of_packet packet =
  let output = Buffer.create 64 in
  Encode.encode_packet output packet;
  Buffer.contents output

let to_packet data =
  let input = Input.({data; pos = 0}) in
  Decode.decode_packet input
