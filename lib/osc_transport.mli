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
    val recv_string : t -> (string * sockaddr) Io.t
  end
end

module Make : functor (T : TRANSPORT) -> sig
  module Client : sig
    type t

    val create : unit -> t T.Io.t

    val destroy : t -> unit T.Io.t

    val send : t -> T.sockaddr -> Osc.packet -> unit T.Io.t
  end

  module Server : sig
    type t

    val create : T.sockaddr -> int -> t T.Io.t

    val destroy : t -> unit T.Io.t

    val recv : t -> (Osc.packet * T.sockaddr) T.Io.t
  end
end
