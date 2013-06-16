type argument =
  | Float32 of float
  | Int32 of int32
  | Str of string
  | Blob of string

type message = {
  address: string;
  arguments: argument list;
}

type packet =
  | Message of message
  | Bundle of packet list

exception Missing_typetags
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

let encode_float32 f =
  BITSTRING {
    Int32.bits_of_float f : 32 : bigendian
  }

let encode_int32 i =
  BITSTRING {
    i : 32 : bigendian
  }

let encode_string s =
  (* Add nulls to pad the string length out to a multiple of
   * four bytes, then convert to a bitstring. *)
  let length = String.length s in
  let suffix = String.make (string_padding_of_length length) '\000' in
  Bitstring.bitstring_of_string (s ^ suffix)

let encode_blob b =
  let length = String.length b in
  let padding = blob_padding_of_length length in
  if padding = 0 then
    BITSTRING {
      (Int32.of_int length) : 32 : bigendian;
      b : length * 8 : string
    }
  else
    let suffix = String.make padding '\000' in
    BITSTRING {
      (Int32.of_int length) : 32 : bigendian;
      b : length * 8 : string;
      suffix : padding * 8 : string
    }

let encode_argument = function
  | Float32 f -> 'f', encode_float32 f
  | Int32 i -> 'i', encode_int32 i
  | Str s -> 's', encode_string s
  | Blob b -> 'b', encode_blob b

let encode_arguments arguments =
  let rec encode_arguments' typetags encoded_acc arguments =
    match arguments with
    | [] ->
      let encoded_typetags = encode_string (Buffer.contents typetags) in
      Bitstring.concat [encoded_typetags; encoded_acc]
    | argument :: rest ->
      let typetag, encoded_argument = encode_argument argument in
      Buffer.add_char typetags typetag;
      encode_arguments'
        typetags
        (Bitstring.concat [encoded_acc; encoded_argument])
        rest
  in
  let argument_count = List.length arguments in
  let typetags = Buffer.create (argument_count + 1) in
  Buffer.add_char typetags ',';
  encode_arguments' typetags Bitstring.empty_bitstring arguments

let read_float32 data =
  bitmatch data with {
    i : 4*8 : bigendian;
    rest : -1 : bitstring
  } ->
    Int32.float_of_bits i, rest

let read_int32 data =
  bitmatch data with {
    i : 4*8 : bigendian;
    rest : -1 : bitstring
  } -> i, rest

let read_string data =
  let rec read_string' buffer data =
    bitmatch data with {
      s : 4*8 : string;
      rest : -1 : bitstring
    } ->
      try
        (* If there's a null, we're at the end. Append everything up the
         * first zero to the buffer, and return the buffer contents. *)
        let index = String.index s '\000' in
        Buffer.add_string buffer (String.sub s 0 index);
        Buffer.contents buffer, rest
      with Not_found ->
        (* No null found, so append all four bytes to the buffer and
         * recursively look at the next four bytes in the bitstring. *)
        Buffer.add_string buffer s;
        read_string' buffer rest
  in
  read_string' (Buffer.create 16) data

let read_blob data =
  let size, rest = read_int32 data in
  let blob_length = Int32.to_int size in
  let padding_length = blob_padding_of_length blob_length in
  bitmatch rest with {
    b : blob_length * 8 : string;
    _ : padding_length * 8 : string;
    rest : -1 : bitstring
  } -> b, rest

let read_argument typetag data =
  match typetag with
  | 'f' -> let f, rest = read_float32 data in Float32 f, rest
  | 'i' -> let i, rest = read_int32 data in Int32 i, rest
  | 's' -> let s, rest = read_string data in Str s, rest
  | 'b' -> let b, rest = read_blob data in Blob b, rest
  | c -> raise (Unsupported_typetag c)

let read_arguments data =
  let rec read_arguments' typetags typetags_count typetags_left data acc =
    if typetags_left <= 0
    then acc, data
    else begin
      let typetag = typetags.[typetags_count - typetags_left + 1] in
      let argument, rest = read_argument typetag data in
      read_arguments'
        typetags
        typetags_count
        (typetags_left - 1)
        rest
        (argument :: acc)
    end
  in
  let typetags, rest = read_string data in
  if typetags.[0] <> ',' then raise Missing_typetags;
  let typetags_count = (String.length typetags) - 1 in
  let typetags_left = typetags_count in
  let arguments, rest =
    read_arguments' typetags typetags_count typetags_left rest [] in
  List.rev arguments, rest
