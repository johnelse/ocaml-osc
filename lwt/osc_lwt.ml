module Io = struct
  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
  let return x = Lwt.return x

  let raise_exn = Lwt.fail

  type input = Lwt_unix.file_descr
  let read = Lwt_unix.read

  type output = Lwt_unix.file_descr
  let write = Lwt_unix.write
end

module Codec = Osc_codec.Make(Io)
