type input = {
  data: string;
  mutable pos: int;
}

exception Missing_typetag_string
exception Not_implemented
exception Unsupported_typetag of char

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
  let int32 input =
    let result = EndianString.BigEndian.get_int32 input.data input.pos in
    input.pos <- input.pos + int32_chars;
    result

  let float32 input =
    Int32.float_of_bits (int32 input)

  let string input =
    let end_pos = String.index_from input.data input.pos '\000' in
    let string_length = end_pos - input.pos in
    let padding_length = string_padding_of_length string_length in
    let result = String.sub input.data input.pos string_length in
    input.pos <- input.pos + string_length + padding_length;
    result

  let blob input =
    let blob_length = Int32.to_int (int32 input) in
    let padding_length = blob_padding_of_length blob_length in
    let result = String.sub input.data input.pos blob_length in
    input.pos <- input.pos + blob_length + padding_length;
    result

  let argument input = function
    | 'f' -> Osc.Float32 (float32 input)
    | 'i' -> Osc.Int32 (int32 input)
    | 's' -> Osc.String (string input)
    | 'b' -> Osc.Blob (blob input)
    | typetag -> raise (Unsupported_typetag typetag)

  let arguments input =
    let typetag_string = string input in
    if typetag_string.[0] <> ','
    then raise Missing_typetag_string
    else begin
      let typetag_count = (String.length typetag_string) - 1 in
      let rec decode typetag_position acc =
        if typetag_position > typetag_count
        then acc
        else
          let arg = argument input typetag_string.[typetag_position] in
          decode (typetag_position + 1) (arg :: acc)
      in
      decode 1 [] |> List.rev
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
      let args = arguments input in
      Osc.(Message {address = address; arguments = args})
end

module Encode = struct
  let int32 output i =
    let tmp = String.create int32_chars in
    EndianString.BigEndian.set_int32 tmp 0 i;
    Buffer.add_string output tmp

  let float32 output f =
    int32 output (Int32.bits_of_float f)

  let string output s =
    Buffer.add_string output s;
    let string_length = String.length s in
    let padding_length = string_padding_of_length string_length in
    let padding = String.make padding_length '\000' in
    Buffer.add_string output padding

  let blob output b =
    let blob_length = String.length b in
    int32 output (Int32.of_int blob_length);
    Buffer.add_string output b;
    let padding_length = blob_padding_of_length blob_length in
    if padding_length > 0 then begin
      let padding = String.make padding_length '\000' in
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
    let typetag_string = String.create ((List.length args) + 1) in
    typetag_string.[0] <- ',';
    List.iteri
      (fun index arg -> typetag_string.[index + 1] <- typetag_of_argument arg)
      args;
    string output typetag_string;
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
  let input = {data; pos = 0} in
  Decode.packet input
