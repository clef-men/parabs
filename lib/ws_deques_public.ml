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
  Ws_deque.pop_opt t.(i)

let steal_to t _i j =
  Ws_deque.steal_opt t.(j)
