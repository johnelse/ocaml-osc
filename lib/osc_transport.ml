module type TRANSPORT = sig
  module Io : sig
    type 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    val return : 'a -> 'a t

    val raise_exn : exn -> 'a t
  end

  type sockaddr

  module Client : sig
    type t
    val create : unit -> t Io.t
    val destroy : t -> unit Io.t
    val send_string : t -> sockaddr -> string -> unit Io.t
  end

  module Server : sig
    type t
    val create : sockaddr -> int -> t Io.t
    val destroy : t -> unit Io.t
    val recv_string : t -> string Io.t
  end
end

module Make(T : TRANSPORT) = struct
  open T.Io

  module Client = struct
    type t = T.Client.t

    let create = T.Client.create

    let destroy = T.Client.destroy

    let send client sockaddr packet =
      let data = Osc_string.of_packet packet in
      T.Client.send_string client sockaddr data
  end

  module Server = struct
    type t = T.Server.t

    let create = T.Server.create

    let destroy = T.Server.destroy

    let recv server =
      T.Server.recv_string server >|= Osc_string.to_packet
  end
end
