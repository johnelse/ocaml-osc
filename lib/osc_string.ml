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

  let read input str offset length =
    let data_length = String.length input.data in
    let blit_length =
      if (input.pos + length) > data_length
      then max (input.pos + length - data_length) 0
      else length
    in
    if blit_length > 0 then begin
      String.blit input.data input.pos str offset blit_length;
      input.pos <- input.pos + blit_length
    end;
    blit_length

  type output = Buffer.t

  let write output str offset length =
    Buffer.add_substring output str offset length;
    length
end

module Codec = Osc_codec.Make(Io)

let of_packet packet =
  let output = Buffer.create 64 in
  Codec.Encode.packet output packet;
  Buffer.contents output

let to_packet data =
  let input = {data; pos = 0} in
  Codec.Decode.packet input
