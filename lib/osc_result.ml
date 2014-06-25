type ('a, 'b) t = [
  | `Ok of 'a
  | `Error of 'b
]

let return x = `Ok x

let fail x = `Error x

let bind x f =
  match x with
  | `Ok result -> f result
  | `Error error -> `Error error

let (>>=) x f = bind x f

let map x f =
  match x with
  | `Ok result -> `Ok (f result)
  | `Error error -> `Error error

let (>|=) x f = map x f
