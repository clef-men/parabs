module type S = sig
  type task =
    unit -> unit

  type t
  type scheduler =
    t

  module Job : sig
    type t

    type 'a suspended

    val noop :
      t

    val make :
      scheduler -> task -> t

    val run :
      t -> unit

    val continue :
      'a suspended -> 'a -> t
    val discontinue :
      'a suspended -> exn -> Printexc.raw_backtrace -> t
  end

  val create :
    int -> t

  val submit_job :
    t -> Job.t -> unit

  val submit_task :
    t -> task -> unit

  val yield :
    (t -> 'a Job.suspended -> unit) -> 'a

  val wait_while :
    t -> (unit -> bool) -> unit
  val wait_until :
    t -> (unit -> bool) -> unit

  val kill :
    t -> unit
end

module Make (Ws_hub_base : Ws_hub.BASE) : S
