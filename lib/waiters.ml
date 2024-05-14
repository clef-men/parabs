module Mpmc_queue =
  Saturn.Queue

type waiter =
  Mpsc_waiter.t

type t =
  waiter Mpmc_queue.t

let create =
  Mpmc_queue.create

let rec notify' t =
  match Mpmc_queue.pop t with
  | None ->
      false
  | Some waiter ->
      if Mpsc_waiter.notify waiter then
        notify' t
      else
        true
let notify t =
  notify' t |> ignore
let rec notify_many t n =
  if n <= 0 then
    ()
  else if notify' t then
    notify_many t (n - 1)

let prepare_wait t =
  let waiter = Mpsc_waiter.create () in
  Mpmc_queue.push t waiter ;
  waiter

let cancel_wait t waiter =
  if Mpsc_waiter.notify waiter then
    notify t

let commit_wait _t waiter =
  Mpsc_waiter.wait waiter
