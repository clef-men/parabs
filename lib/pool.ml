module type S = sig
  type task =
    unit -> unit

  type t
  type pool =
    t

  module Job : sig
    type t

    type 'a suspended

    val noop :
      t

    val make :
      pool -> task -> t

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

  val wait_until :
    t -> (unit -> bool) -> unit

  val kill :
    t -> unit
end

module Make (Ws_hub_base : Ws_hub.BASE) : S = struct
  module Ws_hub =
    Ws_hub.Make (Ws_hub_base)

  type task =
    unit -> unit

  module Job : sig
    type t

    type 'a suspended

    type pool =
      { dls: int Domain.DLS.key;
        hub: t Ws_hub.t;
        domains: unit Domain.t array;
      }

    type _ Effect.t +=
      | Yield : (pool -> 'a suspended -> unit) -> 'a Effect.t

    val noop :
      t

    val make :
      pool -> task -> t

    val run :
      t -> unit

    val continue :
      'a suspended -> 'a -> t
    val discontinue :
      'a suspended -> exn -> Printexc.raw_backtrace -> t
  end = struct
    type t =
      unit -> unit

    type 'a suspended =
      ('a, unit) Effect.Deep.continuation

    type pool =
      { dls: int Domain.DLS.key;
        hub: t Ws_hub.t;
        domains: unit Domain.t array;
      }

    type _ Effect.t +=
      | Yield : (pool -> 'a suspended -> unit) -> 'a Effect.t

    let noop () =
      ()

    let make pool task () =
      Effect.Deep.try_with task () { effc =
        fun (type a) (eff : a Effect.t) : ((a, _) Effect.Deep.continuation -> _) Option.t ->
          match eff with
          | Yield handler ->
              Some (handler pool)
          | _ ->
              None
      }

    let[@inline] run t =
      t ()

    let[@inline] continue k x () =
      Effect.Deep.continue k x
    let[@inline] discontinue k exn bt () =
      Effect.Deep.discontinue_with_backtrace k exn bt
  end

  type t = Job.pool =
    { dls: int Domain.DLS.key;
      hub: Job.t Ws_hub.t;
      domains: unit Domain.t array;
    }
  type pool =
    t

  let max_round_noyield =
    1024
  let max_round_yield =
    32

  let rec worker hub id =
    match Ws_hub.pop_steal hub id ~max_round_noyield ~max_round_yield with
    | None ->
        ()
    | Some task ->
        Job.run task ;
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

  let submit_job t job =
    Ws_hub.push t.hub (Domain.DLS.get t.dls) job

  let submit_task t task =
    submit_job t (Job.make t task)

  let[@inline] yield handler =
    Effect.perform (Job.Yield handler)

  let rec wait_until t cond =
    if not @@ cond () then (
      begin match Ws_hub.pop_try_steal t.hub 0 ~max_round_noyield ~max_round_yield with
      | None ->
          Domain.cpu_relax ()
      | Some job ->
          Job.run job
      end ;
      wait_until t cond
    )
  let wait_until t cond =
    if Ws_hub.killed t.hub then
      invalid_arg @@ __FUNCTION__ ^ ": pool already killed" ;
    wait_until t cond

  let kill t =
    Ws_hub.kill t.hub ;
    Array.iter Domain.join t.domains
end
