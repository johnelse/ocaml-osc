module Udp : sig
  module Client : sig
    type t

    val create : unit -> t

    val destroy : t -> unit

    val send : t -> Unix.sockaddr -> Osc.packet -> unit
  end

  module Server : sig
    type t

    val create : Unix.sockaddr -> int -> t

    val destroy : t -> unit

    val recv : t -> Osc.packet * Unix.sockaddr
  end
end
