module Make (Ws_deques : Ws_deques.S) : sig
  type t
  type _ promise

  val make : Int.t -> t

  val size : t -> Int.t

  val yield : Unit.t -> Unit.t
  val async : t -> (Unit.t -> 'a) -> 'a promise
  val await : 'a promise -> 'a

  val run : t -> (Unit.t -> 'a) -> 'a
  val kill : t -> Unit.t
end
