type 'a t =
  'a Clist.t Atomic.t

let create () =
  Atomic.make Clist.Open

let rec push t v =
  match Atomic.get t with
  | Clist.Closed ->
      true
  | _ as old ->
      let new_ = Clist.Cons (v, old) in
      if Atomic.compare_and_set t old new_ then (
        false
      ) else (
        Domain.cpu_relax () ;
        push t v
      )

let rec pop t =
  match Atomic.get t with
  | Clist.Closed ->
      Option2.None2
  | Open ->
      Option2.None1
  | Cons (v, new_) as old ->
      if Atomic.compare_and_set t old new_ then (
        Option2.Some2 v
      ) else (
        Domain.cpu_relax () ;
        pop t
      )

let is_closed t =
  Atomic.get t == Clist.Closed

let close t =
  Atomic.exchange t Clist.Closed
