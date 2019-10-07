module Io = struct
  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
  let return x = Lwt.return x
end

module UdpTransport = struct
  module Io = Io
  open Io

  type sockaddr = Lwt_unix.sockaddr

  module Client = struct
    type t = {
      socket: Lwt_unix.file_descr;
    }

    let create () =
      let socket = Lwt_unix.socket
        Lwt_unix.PF_INET
        Lwt_unix.SOCK_DGRAM
        (Unix.getprotobyname "udp").Unix.p_proto
      in
      return {socket}

    let destroy client =
      Lwt_unix.close client.socket

    let send_string client addr data =
      let length = String.length data in
      Lwt_unix.sendto client.socket
        (Bytes.unsafe_of_string data) 0 length [] addr
      >>= (fun sent ->
        if sent <> length
        then Lwt.fail (Failure "IO error")
        else return ())
  end

  module Server = struct
    type t = {
      buffer_length: int;
      buffer: bytes;
      socket: Lwt_unix.file_descr;
    }

    let create addr buffer_length =
      let buffer = Bytes.create buffer_length in
      Lwt_unix.getprotobyname "udp"
      >>= (fun proto ->
        let socket = Lwt_unix.socket
          Lwt_unix.PF_INET
          Lwt_unix.SOCK_DGRAM
          proto.Lwt_unix.p_proto
        in
        Lwt_unix.bind socket addr
        >>= (fun () -> return {buffer_length; buffer; socket}))

    let destroy server =
      Lwt_unix.close server.socket

    let recv_string server =
      Lwt_unix.recvfrom server.socket server.buffer 0 server.buffer_length []
      >>= (fun (length, sockaddr) ->
        return
          (Bytes.unsafe_to_string (Bytes.sub server.buffer 0 length), sockaddr))
  end
end

module Udp = Osc.Transport.Make(UdpTransport)
