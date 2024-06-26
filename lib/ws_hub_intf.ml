module type BASE = sig
  type 'a t

  val create :
    int -> 'a t

  val push :
    'a t -> int -> 'a -> unit

  val pop :
    'a t -> int -> 'a option

  val steal_until :
    max_round_noyield:int ->
    'a t -> int -> (unit -> bool) -> 'a option

  val steal :
    max_round_noyield:int ->
    max_round_yield:int ->
    'a t -> int -> 'a option

  val killed :
    'a t -> bool

  val kill :
    'a t -> unit
end

module type S = sig
  include BASE

  val pop_steal_until :
    max_round_noyield:int ->
    'a t -> int -> (unit -> bool) -> 'a option

  val pop_steal :
    max_round_noyield:int ->
    max_round_yield:int ->
    'a t -> int -> 'a option
end
