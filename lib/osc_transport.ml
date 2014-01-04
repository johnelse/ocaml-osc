module type IO = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val return : 'a -> 'a t

  val raise_exn : exn -> 'a t

  type input
  val read_char : input -> char t
  val read_string : input -> string -> int -> int -> int t
  val read_int32 : input -> int32 t

  type output
  val write_char : output -> char -> unit t
  val write_string : output -> string -> unit t
  val write_int32 : output -> Int32.t -> unit t
end
