open Ws_deques

module Ws_deque = Lockfree.Ws_deque.M

type 'a task =
  | Task of 'a
  | Kill

type _ dls =
  Unit.t

type 'a t = {
  deques : 'a task Ws_deque.t Array.t ;
}

let dls_make _id =
  ()
let make sz =
  {deques = Array.init sz (fun _ -> Ws_deque.create ())}

let push t id _dls task =
  Ws_deque.push (Array.unsafe_get t.deques id) (Task task)

let pop t id _dls =
  begin match Ws_deque.pop (Array.unsafe_get t.deques id) with
  | exception Exit ->
      Error Fail
  | Task task ->
      Ok task
  | Kill ->
      Error Kill
  end

let steal_pre _t _id _dls =
  ()
let steal t _id _dls victim_id =
  begin match Ws_deque.steal (Array.unsafe_get t.deques victim_id) with
  | exception Exit ->
      Error Fail
  | Task task ->
      Ok task
  | Kill ->
      Error Kill
  end
let steal_post _t _id _dls =
  ()

let kill t id _dls =
  let deque = Array.unsafe_get t.deques id in
  for _ = 0 to Array.length t.deques - 1 do
    Ws_deque.push deque Kill
  done
