type 'a t =
  { mutable prev: 'a t;
    mutable next: 'a t;
    data: 'a;
  }

let create () =
  let rec t = { prev= t; next= t; data= Obj.magic () } in
  t

let[@inline] is_empty t =
  t.next == t

let insert prev v next =
  let node = { prev; next; data= v } in
  prev.next <- node ;
  next.prev <- node

let push_front t v =
  insert t v t.next

let push_back t v =
  insert t.prev v t

let pop_front t =
  if is_empty t then (
    None
  ) else (
    let old_front = t.next in
    let front = old_front.next in
    front.prev <- t ;
    t.next <- front ;
    Some old_front.data
  )

let pop_back t =
  if is_empty t then (
    None
  ) else (
    let old_back = t.prev in
    let back = old_back.prev in
    t.prev <- back ;
    back.next <- t ;
    Some old_back.data
  )
