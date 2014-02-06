module Io = struct
  type 'a t = 'a
  let (>>=) x f = f x
  let (>|=) x f = f x
  let return x = x

  let raise_exn = raise

  type input = Unix.file_descr
  let read = Unix.read

  type output = Unix.file_descr
  let write = Unix.write
end

module Codec = Osc_codec.Make(Io)
