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

module Make (Io : Osc_transport.IO) = struct
  open Io

  module Encode = struct
    let float32 output f =
      Io.write_int32 output (Int32.bits_of_float f)

    let int32 output i =
      Io.write_int32 output i

    let string output s =
      let length = String.length s in
      let suffix = String.make (string_padding_of_length length) '\000' in
      Io.write_string output (s ^ suffix)

    let blob output b =
      let length = String.length b in
      let suffix = String.make (blob_padding_of_length length) '\000' in
      Io.write_string output (b ^ suffix)
  end

  module Decode = struct
    let float32 input =
      Io.read_int32 input >|= Int32.float_of_bits

    let int32 input =
      Io.read_int32 input

    let string input =
      let rec read buffer =
        let data = String.create 4 in
        Io.read_string input data 0 4
        >>= (function
          | 4 -> begin
            try
              (* If there's a null, we're at the end. Append everything up the
               * first null to the buffer, and return the buffer contents. *)
              let index = String.index data '\000' in
              Buffer.add_string buffer (String.sub data 0 index);
              Io.return (Buffer.contents buffer)
            with Not_found ->
              (* No null found, so append all four bytes to the buffer and
               * recursively read the next four bytes. *)
              Buffer.add_string buffer data;
              read buffer
          end
          | _ -> Io.raise_exn End_of_file)
      in
      read (Buffer.create 16)

    (* Read the blob length, then use that to read the blob and the padding.
     * Return the substring containing just the blob data. *)
    let blob input =
      Io.read_int32 input
      >>= (fun size ->
        let blob_length = Int32.to_int size in
        let padding_length = blob_padding_of_length blob_length in
        let total_length = blob_length + padding_length in
        let data = String.create total_length in
        Io.read_string input data 0 total_length
        >>= (function
          | chars_read when chars_read = total_length ->
            Io.return (String.sub data 0 blob_length)
          | _ -> Io.raise_exn End_of_file))
  end
end

let encode_float32 f =
  BITSTRING {
    Int32.bits_of_float f : 32 : bigendian
  }

let encode_int32 i =
  BITSTRING {
    i : 32 : bigendian
  }

(* Add 1-4 nulls to pad the string length out to a multiple of
 * four bytes, then convert to a bitstring. *)
let encode_string s =
  let length = String.length s in
  let suffix = String.make (string_padding_of_length length) '\000' in
  Bitstring.bitstring_of_string (s ^ suffix)

(* Encode the length of the blob, followed by the blob itself (as a string),
 * followed by 0-3 nulls to make the total length a multiple of 4 bytes. *)
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

(* Recursively create a string of typetags and a bitstring containing all
 * the encoded arguments. At the end, concat these into a single bitstring. *)
let encode_arguments arguments =
  let rec encode_arguments' typetags encoded_arguments arguments =
    match arguments with
    | [] ->
      let encoded_typetags = encode_string (Buffer.contents typetags) in
      Bitstring.concat [encoded_typetags; encoded_arguments]
    | argument :: rest ->
      let typetag, encoded_argument = encode_argument argument in
      Buffer.add_char typetags typetag;
      encode_arguments'
        typetags
        (Bitstring.concat [encoded_arguments; encoded_argument])
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
         * first null to the buffer, and return the buffer contents. *)
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

(* Read the blob length, then use that to read the blob. Work out the expected
 * number of padding characters and discard them, and return the rest of the
 * input bitstring. *)
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

(* Read the typetag string, then use that to read the arguments one-by-one. *)
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
