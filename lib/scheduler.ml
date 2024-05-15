let ignore_exceptions fn arg =
  try fn arg with _ -> ()

module type S = sig
  type 'a task =
    unit -> 'a

  type t
  type scheduler =
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
      scheduler -> t -> unit task -> unit

    val yield :
      t -> unit

    val spawn :
      scheduler -> t -> unit task -> unit
  end
end

module Make (Ws_hub_base : Ws_hub.BASE) : S = struct
  module Pool =
    Pool.Make (Ws_hub_base)
  module Job =
    Pool.Job

  type 'a task =
    unit -> 'a

  type t =
    Pool.t
  type scheduler =
    t

  type 'a state =
    | Returned of 'a
    | Raised of Printexc.t * Printexc.raw_backtrace
    | Pending of 'a Job.suspended list
  type 'a future =
    'a state Atomic.t

  let create =
    Pool.create

  let silent_async t task =
    Pool.submit_task t (ignore_exceptions task)

  let async t fut task () =
    let res, job =
      match task () with
      | v ->
          Returned v,
          fun suspended -> Job.continue suspended v
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          Raised (exn, bt),
          fun suspended -> Job.discontinue suspended exn bt
    in
    match Atomic.exchange fut res with
    | Pending suspendeds ->
        List.iter (fun suspended -> Pool.submit_job t (job suspended)) suspendeds
    |  _ ->
        failwith @@ __FUNCTION__ ^ ": impossible: cannot set future result more than once"
  let async t task =
    let fut = Atomic.make (Pending []) in
    Pool.submit_task t (async t fut task) ;
    fut

  let rec await fut suspended =
    match Atomic.get fut with
    | Returned v ->
        Job.(run @@ continue suspended v)
    | Raised (exn, bt) ->
        Job.(run @@ discontinue suspended exn bt)
    | Pending suspendeds as state ->
        if not @@ Atomic.compare_and_set fut state @@ Pending (suspended :: suspendeds) then (
          Domain.cpu_relax () ;
          await fut suspended
        )
  let await fut _t suspended =
    await fut suspended
  let await fut =
    match Atomic.get fut with
    | Returned v ->
        v
    | Raised (exn, bt) ->
        Printexc.raise_with_backtrace exn bt
    | Pending _ ->
        Pool.yield (await fut)

  let yield t suspended =
    Pool.submit_job t (Job.continue suspended ())
  let yield () =
    Pool.yield yield

  let run t task =
    let fut = async t task in
    Pool.wait_until t (fun () ->
      match Atomic.get fut with
      | Pending _ ->
          false
      | _ ->
          true
    ) ;
    match Atomic.get fut with
    | Returned v ->
        v
    | Raised (e, bt) ->
        Printexc.raise_with_backtrace e bt
    | Pending _ ->
        failwith @@ __FUNCTION__ ^ ": impossible: unset future"

  let kill =
    Pool.kill

  module Vertex = struct
    type t =
      { mutable job: Job.t;
        preds: int Atomic.t;
        succs: t Mpmc_stack.t;
      }

    let create () =
      { job= Job.noop;
        preds= Atomic.make 1;
        succs= Mpmc_stack.create ();
      }

    let precede t1 t2 =
      if not @@ Mpmc_stack.is_closed t1.succs then (
        Atomic.incr t2.preds ;
        if Mpmc_stack.push t1.succs t2 then
          Atomic.decr t2.preds
      )

    let propagate sched t =
      let preds = Atomic.fetch_and_add t.preds (-1) in
      if preds <= 0 then
        failwith @@ __FUNCTION__ ^ ": illegal state probably due to multiple vertex releases" ;
      if preds = 1 then (
        Atomic.incr t.preds ;
        Pool.submit_job sched t.job
      )

    let run sched t task () =
      ignore_exceptions task () ;
      Clist.iter (Mpmc_stack.close t.succs) (propagate sched)
    let release sched t task =
      t.job <- Job.make sched (run sched t task) ;
      propagate sched t

    let yield t =
      Pool.yield (fun sched suspended ->
        t.job <- Job.continue suspended () ;
        propagate sched t
      )

    let spawn sched t task =
      let t' = create () in
      precede t' t ;
      release sched t' task
  end
end
