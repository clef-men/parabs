module type S = sig
  type t

  type context

  type 'a task =
    context -> 'a

  type 'a future

  val create :
    int -> t

  val run :
    t -> 'a task -> 'a

  val silent_async :
    context -> unit task -> unit

  val async :
    context -> 'a task -> 'a future

  val await :
    context -> 'a future -> 'a

  val kill :
    t -> unit

  module Vertex : sig
    type t

    val create :
      unit task -> t

    val precede :
      t -> t -> unit

    val release :
      context -> t -> unit
  end
end

module Make (Ws_hub_base : Ws_hub.BASE) : S
