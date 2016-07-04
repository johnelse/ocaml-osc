(** OSC transport library using Lwt IO. *)

(** OSC communication over UDP. *)
module Udp : sig
  module Client : sig
    type t
    (** The type of a UDP Lwt client. *)

    val create : unit -> t Lwt.t
    (** Create a new client. *)

    val destroy : t -> unit Lwt.t
    (** Destroy a client. *)

    val send : t -> Lwt_unix.sockaddr -> Osc.Types.packet -> unit Lwt.t
    (** Send an OSC packet to a specified address. *)
  end

  module Server : sig
    type t
    (** The type of a UDP Lwt server. *)

    val create : Lwt_unix.sockaddr -> int -> t Lwt.t
    (** Create a new server. *)

    val destroy : t -> unit Lwt.t
    (** Destroy a server. *)

    val recv :
      t ->
      ((Osc.Types.packet * Lwt_unix.sockaddr, [
        | `Missing_typetag_string
        | `Unsupported_typetag of char
      ]) Result.result) Lwt.t
    (** Attempt to receive the next packet. *)
  end
end
