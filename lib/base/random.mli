include module type of Stdlib.Random

module Round : sig
  type 'a t

  val make : Int.t -> (Int.t -> 'a) -> 'a t
  val reset : 'a t -> Unit.t
  val next : 'a t -> 'a
  val next_reset : 'a t -> 'a
end
