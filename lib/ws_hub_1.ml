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

  let rec try_steal ~yield ~max_round t i =
    if max_round <= 0 then
      None
    else
      match pop_foreign t with
      | Some _ as res ->
          res
      | None ->
          match try_steal_once t i with
          | Some _ as res ->
              res
          | None ->
              if yield then
                Domain.cpu_relax () ;
              try_steal t i ~yield ~max_round:(max_round - 1)
  let try_steal ~max_round_noyield ~max_round_yield t i =
    match try_steal t i ~yield:false ~max_round:max_round_noyield with
    | Some _ as res ->
        res
    | None ->
        try_steal t i ~yield:true ~max_round:max_round_yield

  let rec steal ~max_round_noyield ~max_round_yield t i =
    match try_steal t i ~max_round_noyield ~max_round_yield with
    | Some _ as res ->
        res
    | None ->
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
