module type S = sig
  type 'a task =
    unit -> 'a

  type t
  type pool =
    t

  type 'a future

  val create :
    int -> t

  val silent_async :
    t -> unit task -> unit

  val async :
    t -> 'a task -> 'a future

  val await :
    'a future -> 'a

  val yield :
    unit -> unit

  val run :
    t -> 'a task -> 'a

  val kill :
    t -> unit

  module Vertex : sig
    type t

    val create :
      unit -> t

    val precede :
      t -> t -> unit

    val release :
      pool -> t -> unit task -> unit

    val yield :
      t -> unit

    val spawn :
      pool -> t -> unit task -> unit
  end
end
