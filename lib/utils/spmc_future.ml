type 'a t =
  { result: 'a option Atomic.t;
    mutex: Mutex.t;
    condition: Condition.t;
  }

let create () =
  { result= Atomic.make None;
    mutex= Mutex.create ();
    condition= Condition.create ();
  }

let set t v =
  Atomic.set t.result (Some v) ;
  Condition.broadcast t.condition

let try_get t =
  Atomic.get t.result

let get t =
  match try_get t with
  | Some v ->
      v
  | None ->
      let result = t.result in
      let mtx = t.mutex in
      let cond = t.condition in
      Mutex.protect mtx (fun () ->
        Condition.wait_while cond mtx (fun () ->
          Atomic.get result = None
        )
      ) ;
      match Atomic.get result with
      | Some v ->
          v
      | None ->
          assert false
