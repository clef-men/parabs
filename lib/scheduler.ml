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

module Make (Ws_hub_base : Ws_hub.BASE) : S = struct
  module Ws_hub =
    Ws_hub.Make (Ws_hub_base)

  type 'a task =
    context -> 'a
  and t =
    { hub: unit task Ws_hub.t;
      domains: unit Domain.t array;
    }
  and context =
    { context_hub: unit task Ws_hub.t;
      context_id: int;
    }

  type 'a future =
    'a Spmc_future.t

  let max_round_noyield =
    1024
  let max_round_yield =
    32

  let execute ctx task =
    task ctx

  let rec worker ctx =
    match Ws_hub.pop_steal ctx.context_hub ctx.context_id max_round_noyield max_round_yield with
    | None ->
        ()
    | Some task ->
        execute ctx task ;
        worker ctx

  let create sz =
    if sz < 0 then
      invalid_arg @@ __FUNCTION__ ^ ": size must be positive" ;
    let hub = Ws_hub.create (1 + sz) in
    let doms =
      Array.init sz @@ fun i ->
        Domain.spawn @@ fun _ ->
          worker { context_hub= hub; context_id= 1 + i }
    in
    { hub; domains= doms }

  let run t task =
    execute { context_hub= t.hub; context_id= 0 } task

  let silent_async ctx task =
    Ws_hub.push ctx.context_hub ctx.context_id task

  let async ctx task =
    let fut = Spmc_future.create () in
    silent_async ctx (fun ctx ->
      Spmc_future.set fut (task ctx)
    ) ;
    fut

  let rec await ctx fut =
    match Spmc_future.try_get fut with
    | Some res ->
        res
    | None ->
        begin match Ws_hub.pop_try_steal ctx.context_hub ctx.context_id max_round_noyield max_round_yield with
        | None ->
            ()
        | Some task ->
            execute ctx task
        end ;
        await ctx fut

  let kill t =
    Ws_hub.kill t.hub ;
    Array.iter Domain.join t.domains

  module Vertex = struct
    type t =
      { task: unit task;
        preds: int Atomic.t;
        succs: t Mpmc_stack.t;
      }

    let create task =
      { task;
        preds= Atomic.make 1;
        succs= Mpmc_stack.create ();
      }

    let precede t1 t2 =
      Atomic.incr t2.preds ;
      if Mpmc_stack.push t1.succs t2 then
        Atomic.decr t2.preds

    let propagate ctx t run =
      if Atomic.fetch_and_add t.preds (-1) = 1 then
        silent_async ctx (fun ctx -> run ctx t)
    let rec run ctx t =
      t.task ctx ;
      Clist.iter (Mpmc_stack.close t.succs) (fun t' ->
        propagate ctx t' run
      )
    let release ctx t =
      propagate ctx t run
  end
end
