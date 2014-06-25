(** Generic OSC transport library, functorised over the
    {{:Osc_transport.TRANSPORT.html}TRANSPORT} module. *)

(** Type of modules which can be used to create an OSC library, via the
    {{:Osc_transport.Make.html}Osc_transport.Make} functor. *)
module type TRANSPORT = sig

  module Io : sig
    type 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    val return : 'a -> 'a t

    val raise_exn : exn -> 'a t
  end

  type sockaddr
  (** A generic socket type. *)

  (** Types and functions for handling clients for this type of transport. *)
  module Client : sig

    type t
    (** A type encapsulating a client for this type of transport, and its
        state. *)

    val create : unit -> t Io.t
    (** Create a new transport client. *)

    val destroy : t -> unit Io.t
    (** Destroy a transport client. *)

    val send_string : t -> sockaddr -> string -> unit Io.t
    (** [send_string client addr data] uses [client] to send [data] to an OSC
        server listening on address [sockaddr]. *)
  end

  (** Types and functions for handling servers for this type of transport. *)
  module Server : sig

    type t
    (** A type encapsulating a server for this type of transport, and its
        state. *)

    val create : sockaddr -> int -> t Io.t
    (** [create addr buffer_length] creates a new server listening on
        [sockaddr]. The server will be able to receive messages of length up
        to [buffer_length]. *)

    val destroy : t -> unit Io.t
    (** Destroy a transport server. *)

    val recv_string : t -> (string * sockaddr) Io.t
    (** Retrieve data sent to the server, as well as the sending
        client's address. *)
  end
end

(** Create OSC client/server modules from a module of type TRANSPORT. *)
module Make : functor (T : TRANSPORT) -> sig

  (** Types and functions for handling OSC clients. *)
  module Client : sig

    type t
    (** An OSC client. *)

    val create : unit -> t T.Io.t
    (** Create a new OSC client. *)

    val destroy : t -> unit T.Io.t
    (** Destroy an OSC client. *)

    val send : t -> T.sockaddr -> Osc.packet -> unit T.Io.t
    (** [send client addr packet] uses [client] to send OSC packet [packet] to
        a server listening at address [addr]. *)
  end

  (** Types and functions for handling OSC servers. *)
  module Server : sig

    type t
    (** An OSC server. *)

    val create : T.sockaddr -> int -> t T.Io.t
    (** [create addr buffer_length] creates a new server listening on
        [sockaddr]. The server will be able to receive messages of length up
        to [buffer_length]. *)

    val destroy : t -> unit T.Io.t
    (** Destroy an OSC server. *)

    val recv :
      t ->
      ((Osc.packet * T.sockaddr, [
        | `Missing_typetag_string
        | `Unsupported_typetag of char
      ]) Osc_result.t) T.Io.t
    (** Retrieve a packet sent to the server, as well as the sending client's
        address. *)
  end
end
