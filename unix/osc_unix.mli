(** OSC transport library using Unix IO. *)

(** OSC communication over UDP. *)
module Udp : sig
  module Client : sig
    type t
    (** The type of a UDP Unix client. *)

    val create : unit -> t
    (** Create a new client. *)

    val destroy : t -> unit
    (** Destroy a client. *)

    val send : t -> Unix.sockaddr -> Osc.Types.packet -> unit
    (** Send an OSC packet to a specified address. *)
  end

  module Server : sig
    type t
    (** The type of a UDP Unix server. *)

    val create : Unix.sockaddr -> int -> t
    (** The type of a UDP Lwt server. *)

    val destroy : t -> unit
    (** Destroy a server. *)

    val recv :
      t ->
      (Osc.Types.packet * Unix.sockaddr, [
        | `Missing_typetag_string
        | `Unsupported_typetag of char
      ]) Result.result
    (** Attempt to receive the next packet. *)
  end
end
