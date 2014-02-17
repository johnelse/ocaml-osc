module Io = struct
  type 'a t = 'a
  let (>>=) x f = f x
  let (>|=) x f = f x
  let return x = x

  let raise_exn = raise

  type file_descr = Unix.file_descr
  type msg_flag = Unix.msg_flag
  type sockaddr = Unix.sockaddr

  let bind = Unix.bind

  let recvfrom = Unix.recvfrom

  let sendto = Unix.sendto
end
