module Mpmc_queue =
  Saturn.Queue

type 'a t =
  { queue: 'a Mpmc_queue.t;
    size: int;
    waiters: Waiters.t;
    mutable killed: bool;
  }

let create sz =
  { queue= Mpmc_queue.create ();
    size= sz;
    waiters= Waiters.create ();
    killed= false;
  }

let size t =
  t.size

let notify t =
  Waiters.notify t.waiters
let notify_all t =
  Waiters.notify_many t.waiters (size t)

let push t _i v =
  Mpmc_queue.push t.queue v ;
  notify t

let pop' t =
  Mpmc_queue.pop t.queue
let pop t _i =
  pop' t

let killed t =
  t.killed

let rec steal_until t pred =
  if pred () then (
    None
  ) else (
    Domain.cpu_relax () ;
    match pop' t with
    | Some _ as res ->
        res
    | None ->
        steal_until t pred
  )
let steal_until ~max_round_noyield:_ t _i pred =
  steal_until t pred

let rec steal t =
  let waiters = t.waiters in
  let waiter = Waiters.prepare_wait waiters in
  if killed t then (
    Waiters.cancel_wait waiters waiter ;
    None
  ) else (
    if Mpmc_queue.is_empty t.queue then (
      Waiters.commit_wait waiters waiter
    ) else (
      Waiters.cancel_wait waiters waiter
    ) ;
    match pop' t with
    | Some _ as res ->
        res
    | None ->
        steal t
  )
let steal ~max_round_noyield:_ ~max_round_yield:_ t _i =
  steal t

let kill t =
  t.killed <- true ;
  notify_all t
