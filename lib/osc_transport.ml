module type IO = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val return : 'a -> 'a t

  val raise_exn : exn -> 'a t

  type input
  val read : input -> string -> int -> int -> int t

  type output
  val write : output -> string -> int -> int -> int t
end
