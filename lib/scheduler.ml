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
      unit task -> t

    val precede :
      t -> t -> unit

    val release :
      scheduler -> t -> unit

    val yield :
      t -> unit
  end
end

module Make (Ws_hub_base : Ws_hub.BASE) : S = struct
  module Ws_hub =
    Ws_hub.Make (Ws_hub_base)

  type 'a task =
    unit -> 'a

  type t =
    { dls: int Domain.DLS.key;
      hub: unit task Ws_hub.t;
      domains: unit Domain.t array;
    }
  type scheduler =
    t

  type 'a state =
    | Returned of 'a
    | Raised of Printexc.t * Printexc.raw_backtrace
    | Pending of ('a, unit) Effect.Deep.continuation list
  type 'a future =
    'a state Atomic.t

  type _ Effect.t +=
    | Yield : (t -> (unit, unit) Effect.Deep.continuation -> unit) -> unit Effect.t
    | Await : 'a future -> 'a Effect.t

  let max_round_noyield =
    1024
  let max_round_yield =
    32

  let rec worker hub id =
    match Ws_hub.pop_steal hub id max_round_noyield max_round_yield with
    | None ->
        ()
    | Some task ->
        task () ;
        worker hub id

  let create sz =
    if sz < 0 then
      invalid_arg @@ __FUNCTION__ ^ ": size must be positive" ;
    let dls = Domain.DLS.new_key @@ fun () -> -1 in
    Domain.DLS.set dls 0 ;
    let hub = Ws_hub.create (1 + sz) in
    let domains =
      Array.init sz @@ fun i ->
        Domain.spawn @@ fun _ ->
          let id = 1 + i in
          Domain.DLS.set dls id ;
          worker hub id
    in
    { dls; hub; domains }

  let[@inline] push_raw t task =
    Ws_hub.push t.hub (Domain.DLS.get t.dls) task

  let rec handle_await fut k =
    match Atomic.get fut with
    | Returned v ->
        Effect.Deep.continue k v
    | Raised (exn, bt) ->
        Effect.Deep.discontinue_with_backtrace k exn bt
    | Pending ks as state ->
        if not @@ Atomic.compare_and_set fut state @@ Pending (k :: ks) then (
          Domain.cpu_relax () ;
          handle_await fut k
        )
  let handle t task () =
    Effect.Deep.try_with task () { effc =
      fun (type a) (eff : a Effect.t) : ((a, _) Effect.Deep.continuation -> _) Option.t ->
        match eff with
        | Yield handle ->
            Some (handle t)
        | Await fut ->
            Some (handle_await fut)
        | _ ->
            None
    }
  let[@inline] push t task =
    task
    |> handle t
    |> push_raw t

  let execute task () =
    match task () with
    | v ->
        v
    | exception _ ->
        ()
  let silent_async t task =
    push t (execute task)

  let execute t fut task () =
    let res, task =
      match task () with
      | v ->
          Returned v,
          fun k () -> Effect.Deep.continue k v
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          Raised (exn, bt),
          fun k () -> Effect.Deep.discontinue_with_backtrace k exn bt
    in
    match Atomic.exchange fut res with
    | Pending ks ->
        List.iter (fun k -> push_raw t (task k)) ks
    |  _ ->
        failwith @@ __FUNCTION__ ^ ": impossible: cannot set future result more than once"
  let async t task =
    let fut = Atomic.make @@ Pending [] in
    push t (execute t fut task) ;
    fut

  let await fut =
    match Atomic.get fut with
    | Returned v ->
        v
    | Raised (exn, bt) ->
        Printexc.raise_with_backtrace exn bt
    | Pending _ ->
        Effect.perform @@ Await fut

  let yield () =
    Effect.perform @@ Yield (fun t k ->
      push_raw t @@ Effect.Deep.continue k
    )

  let rec run t fut =
    match Atomic.get fut with
    | Returned v ->
        v
    | Raised (e, bt) ->
        Printexc.raise_with_backtrace e bt
    | Pending _ ->
        begin match Ws_hub.pop_try_steal t.hub 0 max_round_noyield max_round_yield with
        | None ->
            Domain.cpu_relax ()
        | Some task ->
            task ()
        end ;
        run t fut
  let run t task =
    if Ws_hub.killed t.hub then
      invalid_arg @@ __FUNCTION__ ^ ": scheduler already killed" ;
    run t @@ async t task

  let kill t =
    Ws_hub.kill t.hub ;
    Array.iter Domain.join t.domains

  module Vertex = struct
    type t =
      { mutable task: unit task;
        preds: int Atomic.t;
        succs: t Mpmc_stack.t;
      }

    let create task =
      { task;
        preds= Atomic.make 1;
        succs= Mpmc_stack.create ();
      }

    let precede t1 t2 =
      if not @@ Mpmc_stack.is_closed t1.succs then (
        Atomic.incr t2.preds ;
        if Mpmc_stack.push t1.succs t2 then
          Atomic.decr t2.preds
      )

    let rec propagate sched t =
      let preds = Atomic.fetch_and_add t.preds (-1) in
      if preds <= 0 then
        failwith @@ __FUNCTION__ ^ ": illegal state probably due to multiple vertex releases" ;
      if preds = 1 then
        push_raw sched @@ run sched t
    and run sched t () =
      Atomic.incr t.preds ;
      t.task () ;
      Clist.iter (Mpmc_stack.close t.succs) (propagate sched)

    let release sched t =
      t.task <- handle sched t.task ;
      propagate sched t

    let yield t =
      Effect.perform @@ Yield (fun sched k ->
        t.task <- Effect.Deep.continue k ;
        propagate sched t
      )
  end
end
