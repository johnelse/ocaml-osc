let int32_chars = 4

module Io = struct
  type 'a t = 'a
  let (>>=) x f = f x
  let (>|=) x f = f x
  let return x = x

  let raise_exn = raise

  type input = in_channel

  let read_char = input_char

  let read_string = Pervasives.input

  let read_int32 input =
    let tmp = String.create int32_chars in
    if (read_string input tmp 0 int32_chars) = int32_chars
    then EndianString.BigEndian.get_int32 tmp 0
    else raise End_of_file

  type output = out_channel

  let write_char output c =
    output_char output c;
    flush output

  let write_string output str =
    let length = String.length str in
    Pervasives.output output str 0 length;
    flush output

  let write_int32 output i =
    let tmp = String.create int32_chars in
    EndianString.BigEndian.set_int32 tmp 0 i;
    write_string output tmp;
    flush output
end

module Codec = Osc_codec.Make(Io)
