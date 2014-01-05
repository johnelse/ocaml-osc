type string_input = {
  data: string;
  mutable pos: int;
}

module Io = struct
  type 'a t = 'a
  let (>>=) x f = f x
  let (>|=) x f = f x
  let return x = x

  let raise_exn = raise

  type input = string_input

  let read_char input =
    if input.pos >= (String.length input.data)
    then raise End_of_file
    else begin
      let c = input.data.[input.pos] in
      input.pos <- input.pos + 1;
      c
    end

  let read_string input str offset length =
    let data_length = String.length input.data in
    let blit_length =
      if (input.pos + length) > data_length
      then max (input.pos + length - data_length) 0
      else length
    in
    String.blit input.data input.pos str offset blit_length;
    input.pos <- input.pos + blit_length;
    blit_length

  let read_int32 input =
    let i = EndianString.BigEndian.get_int32 input.data input.pos in
    input.pos <- input.pos + 4;
    i

  type output = Buffer.t

  let write_char = Buffer.add_char

  let write_string = Buffer.add_string

  let write_int32 output i=
    let tmp = String.create 4 in
    EndianString.BigEndian.set_int32 tmp 0 i;
    Buffer.add_string output tmp
end

module Streamer = Osc.Make(Io)
