module Make (Ws_deques_base : Ws_deques.BASE) : Ws_hub.BASE = struct
  module Ws_deques =
    Ws_deques.Make (Ws_deques_base)

  module Mpmc_queue =
    Saturn.Queue

  type 'a t =
    { deques: 'a Ws_deques.t;
      foreign: 'a Mpmc_queue.t;
      rounds: Random_round.t array;
      waiters: Waiters.t;
      mutable killed: bool;
    }

  let create sz =
    { deques= Ws_deques.create sz;
      foreign= Mpmc_queue.create ();
      rounds= Array.init sz (fun _ -> Random_round.create @@ Int.max 0 (sz - 1));
      waiters= Waiters.create ();
      killed= false;
    }

  let size t =
    Array.length t.rounds

  let killed t =
    t.killed

  let notify t =
    Waiters.notify t.waiters
  let notify_all t =
    Waiters.notify_many t.waiters (size t)

  let push t i v =
    Ws_deques.push t.deques i v ;
    notify t

  let push_foreign t v =
    Mpmc_queue.push t.foreign v ;
    notify t

  let pop t i =
    Ws_deques.pop t.deques i

  let pop_foreign t =
    Mpmc_queue.pop t.foreign

  let try_steal_once t i =
    let round = t.rounds.(i) in
    Random_round.reset round ;
    Ws_deques.steal_as t.deques i round

  let rec try_steal ~yield ~max_round ~until t i =
    if max_round <= 0 then
      Optional.Nothing
    else
      match pop_foreign t with
      | Some v ->
          Optional.Something v
      | None ->
          match try_steal_once t i with
          | Some v ->
              Optional.Something v
          | None ->
              if until () then
                Optional.Anything
              else (
                if yield then
                  Domain.cpu_relax () ;
                try_steal t i ~yield ~max_round:(max_round - 1) ~until
              )

  let rec steal_until t i cond =
    match pop_foreign t with
    | Some _ as res ->
        res
    | None ->
        match try_steal_once t i with
        | Some _ as res ->
            res
        | None ->
            if cond () then (
              None
            ) else (
              Domain.cpu_relax () ;
              steal_until t i cond
            )
  let steal_until ~max_round_noyield t i cond =
    match try_steal t i ~yield:false ~max_round:max_round_noyield ~until:cond with
    | Optional.Something v ->
        Some v
    | Anything ->
        None
    | Nothing ->
        steal_until t i cond

  let steal_aux ~max_round_noyield ~max_round_yield ~until t i =
    match try_steal t i ~yield:false ~max_round:max_round_noyield ~until with
    | Optional.Something _ as res ->
        res
    | Anything ->
        Anything
    | Nothing ->
        try_steal t i ~yield:true ~max_round:max_round_yield ~until
  let rec steal ~max_round_noyield ~max_round_yield t i =
    match steal_aux t i ~max_round_noyield ~max_round_yield ~until:(fun () -> killed t) with
    | Optional.Something v ->
        Some v
    | Anything ->
        None
    | Nothing ->
        let waiters = t.waiters in
        let waiter = Waiters.prepare_wait waiters in
        match try_steal_once t i with
        | Some _ as res ->
            Waiters.cancel_wait waiters waiter ;
            res
        | None ->
            if killed t then (
              Waiters.cancel_wait waiters waiter ;
              None
            ) else (
              Waiters.commit_wait waiters waiter ;
              steal t i ~max_round_noyield ~max_round_yield
            )

  let kill t =
    t.killed <- true ;
    notify_all t
end
