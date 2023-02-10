module Make (Ws_deques : Ws_deques.S) : sig
  type t
  type dag = t

  val make : Int.t -> t
  val size : t -> Int.t

  val yield : Unit.t -> Unit.t

  val run : t -> (Unit.t -> Unit.t) -> Unit.t

  module Vertex : sig
    type t
    val self : dag -> t
    val make : dag -> (Unit.t -> Unit.t) -> t
    val release : dag -> t -> Unit.t
    val link : t -> t -> Unit.t
  end

  module Future : sig
    type 'a t
    val make : dag -> (Unit.t -> 'a) -> 'a t
    val force : dag -> 'a t -> 'a
  end
end
