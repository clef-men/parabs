type t =
  { flag: bool Atomic.t;
    mutex: Mutex.t;
    condition: Condition.t;
  }

let create () =
  { flag= Atomic.make false;
    mutex= Mutex.create ();
    condition= Condition.create ();
  }

let notify t =
  let flag = t.flag in
  if Atomic.get flag then (
    true
  ) else (
    let res =
      Mutex.protect t.mutex (fun () ->
        if Atomic.get flag then (
          true
        ) else (
          Atomic.set flag true ;
          false
        )
      )
    in
    Condition.signal t.condition ;
    res
  )

let try_wait t =
  Atomic.get t.flag

let wait t =
  if not @@ try_wait t then (
    let flag = t.flag in
    let mtx = t.mutex in
    let cond = t.condition in
    Mutex.protect mtx (fun () ->
      Condition.wait_until cond mtx (fun () ->
        Atomic.get flag
      )
    )
  )
