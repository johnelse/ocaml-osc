module Io = struct
  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
  let return x = Lwt.return x

  let raise_exn = Lwt.fail

  type file_descr = Lwt_unix.file_descr
  type msg_flag = Lwt_unix.msg_flag
  type sockaddr = Unix.sockaddr

  let bind = Lwt_unix.bind

  let recvfrom = Lwt_unix.recvfrom

  let sendto = Unix.sendto
end
