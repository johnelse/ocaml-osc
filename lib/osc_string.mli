(** Conversion of OSC packets to and from strings. *)

val of_packet : Osc.packet -> string
(** Serialise an OSC packet into a string. *)

val to_packet :
  string ->
  (Osc.packet, [
    | `Missing_typetag_string
    | `Unsupported_typetag of char
  ]) Osc_result.t
(** Attempt to deserialise a string into an OSC packet. *)
