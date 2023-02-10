module Make (Ws_deques : Ws_deques.S) = struct
  type t = {
    ws_deques : (Unit.t -> Unit.t) Ws_deques.t ;
    doms : Unit.t Domain.t Array.t ;
  }

  type 'a promise_state =
    | Returned of 'a
    | Raised of Printexc.t * Printexc.raw_backtrace
    | Pending of ('a, Unit.t) Effect.Deep.continuation List.t
  type 'a promise =
    'a promise_state Atomic.t

  type _ Effect.t +=
    | Yield : Unit.t Effect.t
    | Await : 'a promise -> 'a Effect.t

  let promise_make () =
    Atomic.make @@ Pending []
  let promise_fulfill ws_deques p task () =
    let res, task =
      begin match task () with
      | v ->
          Returned v,
          (fun k () -> Effect.Deep.continue k v)
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          Raised (exn, bt),
          (fun k () -> Effect.Deep.discontinue_with_backtrace k exn bt)
      end
    in
    begin match Atomic.exchange p res with
    | Pending ks ->
        List.iter (fun k -> Ws_deques.push ws_deques @@ task k) ks
    |  _ ->
        failwith @@ __FUNCTION__ ^ ": impossible: cannot set promise result more than once"
    end
  let promise_fulfill ws_deques p task () =
    let task = promise_fulfill ws_deques p task in
    Effect.Deep.try_with task () { effc =
      fun (type a) (eff : a Effect.t) : ((a, _) Effect.Deep.continuation -> _) Option.t ->
        begin match eff with
        | Yield -> Some (fun k ->
            Ws_deques.push ws_deques @@ Effect.Deep.continue k
        )
        | Await p -> Some (fun k ->
            let rec loop () =
              let old_p = Atomic.get p in
              begin match old_p with
              | Returned v ->
                  Effect.Deep.continue k v
              | Raised (exn, bt) ->
                  Effect.Deep.discontinue_with_backtrace k exn bt
              | Pending ks ->
                  if not @@ Atomic.compare_and_set p old_p @@ Pending (k :: ks) then (
                    Domain.cpu_relax () ;
                    loop ()
                  )
              end
            in
            loop ()
        )
        | _ ->
            None
        end
    }

  let yield () =
    Effect.perform Yield

  let async t task =
    let p = promise_make () in
    Ws_deques.push t.ws_deques @@ promise_fulfill t.ws_deques p task ;
    p

  let await p =
    begin match Atomic.get p with
    | Returned v ->
        v
    | Raised (exn, bt) ->
        Printexc.raise_with_backtrace exn bt
    | Pending _ ->
        Effect.perform @@ Await p
    end

  let rec worker ws_deques =
    begin match Ws_deques.pop_steal ws_deques with
    | Some task ->
        task () ;
        worker ws_deques
    | None ->
        ()
    end
  let rec master ws_deques p =
    begin match Atomic.get p with
    | Returned v ->
        v
    | Raised (e, bt) ->
        Printexc.raise_with_backtrace e bt
    | Pending _ ->
        begin match Ws_deques.pop ws_deques with
        | Ok task ->
            task ()
        | Error Fail ->
            Domain.cpu_relax ()
        | Error Kill ->
            failwith @@ __FUNCTION__ ^ ": attempt to kill running domain pool"
        end ;
        master ws_deques p
    end

  let make sz =
    if sz < 0 then (
      invalid_arg @@ __FUNCTION__ ^ ": domain pool size must be positive"
    ) else (
      let ws_deques = Ws_deques.make (sz + 1) in
      let doms = Array.init sz @@ fun _ -> Domain.spawn @@ fun () -> worker ws_deques in
      { ws_deques ; doms }
    )

  let size t =
    Array.length t.doms + 1

  let run t task =
    let p = async t task in
    master t.ws_deques p

  let kill t =
    Ws_deques.kill t.ws_deques ;
    Array.iter Domain.join t.doms
end
