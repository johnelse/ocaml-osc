(** Types representing OSC packets. *)

type argument =
  | Float32 of float
  (** 32-bit float argument. *)
  | Int32 of int32
  (** 32-bit integer argument. *)
  | String of string
  (** String argument. *)
  | Blob of string
  (** Binary blob argument. *)
(** An argument contained by an OSC message. *)

type message = {
  address: string;
  (** The OSC address pattern to which the message is aimed. *)
  arguments: argument list;
  (** The arguments associated with the message. *)
}
(** An OSC message. *)

type time = {
  seconds: int32;
  (** Number of seconds since midnight on January 1st, 1900. *)
  fraction: int32;
  (** Fractional part of the timestamp. *)
}
(** An NTP-style timestamp. *)

type timetag =
  | Immediate
  (** A special time tag value, indicating that associated OSC methods should
      be invoked immediately. *)
  | Time of time
  (** A time at which an OSC method should be invoked. *)
(** An OSC time tag. *)

type bundle = {
  timetag: timetag;
  (** A timetag, representing the time at which any actions triggered by this
      bundle should begin. *)
  packets: packet list;
  (** The list of packets contained within this bundle. *)
}
(** An OSC bundle. *)

and packet =
  | Message of message
  (** A single OSC message. *)
  | Bundle of bundle
  (** An OSC bundle, containing a timetag and zero or more child packets. *)
(** An OSC packet. *)
