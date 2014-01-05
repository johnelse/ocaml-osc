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
      Io.write_int32 output (Int32.of_int length)
      >>= (fun () -> Io.write_string output (b ^ suffix))

    let argument output = function
      | Float32 f -> float32 output f
      | Int32 i -> int32 output i
      | Str s -> string output s
      | Blob b -> blob output b

    let arguments output args =
      let typetag_of_argument = function
        | Float32 _ -> 'f'
        | Int32 _ -> 'i'
        | Str _ -> 's'
        | Blob _ -> 'b'
      in
      (* Encode the typetags as a string. *)
      let typetag_string = String.create (List.length args) in
      List.iteri
        (fun index arg -> typetag_string.[index] <- typetag_of_argument arg)
        args;
      string output typetag_string
        (* Encode each argument. *)
      >>= (fun () ->
        let rec encode = function
          | [] -> Io.return ()
          | arg :: rest ->
            argument output arg >>= (fun () -> encode rest)
        in
        encode args)
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

    let argument input = function
      | 'f' -> (float32 input) >|= (fun f -> Float32 f)
      | 'i' -> (int32 input) >|= (fun i -> Int32 i)
      | 's' -> (string input) >|= (fun s -> Str s)
      | 'b' -> (blob input) >|= (fun b -> Blob b)
      | typetag -> Io.raise_exn (Unsupported_typetag typetag)

    let arguments input =
      string input >>=
        (fun typetag_string ->
          let typetag_count = String.length typetag_string in
          let rec decode typetag_index acc =
            if typetag_index = typetag_count
            then Io.return acc
            else
              argument input typetag_string.[typetag_index]
              >>= (fun arg -> decode (typetag_index + 1) (arg :: acc))
          in
          decode 0 [] >|= List.rev)
  end
end
