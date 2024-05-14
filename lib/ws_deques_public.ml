module Ws_deque =
  Saturn.Work_stealing_deque.M

type 'a t =
  'a Ws_deque.t array

let create sz =
  Array.init sz (fun _ -> Ws_deque.create ())

let size t =
  Array.length t

let push t i v =
  Ws_deque.push t.(i) v

let pop t i =
  let deque = t.(i) in
  try Some (Ws_deque.pop deque) with Exit -> None

let steal_to t _i j =
  let deque = t.(j) in
  try Some (Ws_deque.steal deque) with Exit -> None
