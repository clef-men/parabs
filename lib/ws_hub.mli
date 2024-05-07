module type BASE = sig
  type 'a t

  val create :
    int -> 'a t

  val push :
    'a t -> int -> 'a -> unit

  val push_foreign :
    'a t -> 'a -> unit

  val pop :
    'a t -> int -> 'a option

  val try_steal :
    'a t -> int -> int -> int -> 'a option

  val steal :
    'a t -> int -> int -> int -> 'a option

  val killed :
    'a t -> bool

  val kill :
    'a t -> unit
end

module type S = sig
  include BASE

  val pop_try_steal :
    'a t -> int -> int -> int -> 'a option

  val pop_steal :
    'a t -> int -> int -> int -> 'a option
end

module Make (Base : BASE) : S
