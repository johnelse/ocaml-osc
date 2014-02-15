exception Missing_typetag_string
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

module type Codec = sig
  type 'a t
  type input
  type output

  module Decode : sig
    val message : input -> Osc.message t
  end

  module Encode : sig
    val message : output -> Osc.message -> unit t
  end
end

module Make (Io : Osc_transport.IO)
  : (Codec with
      type 'a t := 'a Io.t and
      type input := Io.input and
      type output := Io.output) =
struct

  open Io

  let int32_chars = 4

  let rec really_read input data offset length =
    if length = 0 then return () else
      Io.read input data offset length
      >>= (fun chars_read ->
        really_read input data
          (offset + chars_read)
          (length - chars_read))

  let read_int32 input =
    let tmp = String.create int32_chars in
    really_read input tmp 0 int32_chars
    >>= (fun () -> return (EndianString.BigEndian.get_int32 tmp 0))

  let rec really_write output data offset length =
    if length = 0 then return () else
      Io.write output data offset length
      >>= (fun chars_written ->
        really_write output data
          (offset + chars_written)
          (length - chars_written))

  let write_int32 output i =
    let tmp = String.create int32_chars in
    EndianString.BigEndian.set_int32 tmp 0 i;
    really_write output tmp 0 int32_chars

  module Encode = struct
    let float32 output f =
      write_int32 output (Int32.bits_of_float f)

    let int32 output i =
      write_int32 output i

    let string output s =
      let length = String.length s in
      let padding_length = string_padding_of_length length in
      let suffix = String.make padding_length '\000' in
      really_write output (s ^ suffix) 0 (length + padding_length)

    let blob output b =
      let length = String.length b in
      let padding_length = blob_padding_of_length length in
      let suffix = String.make padding_length '\000' in
      write_int32 output (Int32.of_int length)
      >>= (fun () -> really_write output (b ^ suffix) 0 (length + padding_length))

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
      string output typetag_string
        (* Encode each argument. *)
      >>= (fun () ->
        let rec encode = function
          | [] -> Io.return ()
          | arg :: rest ->
            argument output arg >>= (fun () -> encode rest)
        in
        encode args)

    let timetag output =
      let open Osc in function
      | Immediate ->
        write_int32 output 0l >>= (fun () -> write_int32 output 1l)
      | Time {seconds; fraction} ->
        write_int32 output seconds
        >>= (fun () -> write_int32 output fraction)

    let message output m =
      string output m.Osc.address
      >>= (fun () -> arguments output m.Osc.arguments)
  end

  module Decode = struct
    let float32 input =
      read_int32 input >|= Int32.float_of_bits

    let int32 input =
      read_int32 input

    let string input =
      let rec read buffer =
        let data = String.create 4 in
        Io.read input data 0 4
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
      read_int32 input
      >>= (fun size ->
        let blob_length = Int32.to_int size in
        let padding_length = blob_padding_of_length blob_length in
        let total_length = blob_length + padding_length in
        let data = String.create total_length in
        Io.read input data 0 total_length
        >>= (function
          | chars_read when chars_read = total_length ->
            Io.return (String.sub data 0 blob_length)
          | _ -> Io.raise_exn End_of_file))

    let argument input = function
      | 'f' -> (float32 input) >|= (fun f -> Osc.Float32 f)
      | 'i' -> (int32 input) >|= (fun i -> Osc.Int32 i)
      | 's' -> (string input) >|= (fun s -> Osc.String s)
      | 'b' -> (blob input) >|= (fun b -> Osc.Blob b)
      | typetag -> Io.raise_exn (Unsupported_typetag typetag)

    let arguments input =
      string input >>=
        (fun typetag_string ->
          if typetag_string.[0] <> ','
          then Io.raise_exn Missing_typetag_string
          else begin
            let typetag_count = (String.length typetag_string) - 1 in
            let rec decode typetag_position acc =
              if typetag_position > typetag_count
              then Io.return acc
              else
                argument input typetag_string.[typetag_position]
                >>= (fun arg -> decode (typetag_position + 1) (arg :: acc))
            in
            decode 1 [] >|= List.rev
          end)

    let timetag input =
      read_int32 input
      >>= (fun seconds -> read_int32 input
      >>= (fun fraction ->
        match seconds, fraction with
        | 0l, 1l -> Io.return Osc.Immediate
        | _ -> Io.return Osc.(Time {seconds; fraction})))

    let message input =
      string input
      >>= (fun address -> arguments input
      >>= (fun args -> Io.return {Osc.address = address; arguments = args}))
  end
end
