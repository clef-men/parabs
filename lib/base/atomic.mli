include module type of Stdlib.Atomic

module Counter : sig
  type t

  val make : Unit.t -> t
  val incr : t -> Int.t
end
