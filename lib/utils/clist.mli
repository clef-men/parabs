type 'a t =
  | Closed
  | Open
  | Cons of 'a * 'a t

val iter :
  'a t -> ('a -> unit) -> unit
