(** Conversion of OSC packets to and from strings. *)

val of_packet : Osc.packet -> string
(** Convert an OSC packet to a string. *)

val to_packet : string -> Osc.packet
(** Attempt to deserialise a string into an OSC packet. *)
