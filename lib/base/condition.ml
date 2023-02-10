include Stdlib.Condition

let [@inline] wait_while t mtx cond =
  Mutex.lock mtx ;
  while cond () do
    wait t mtx
  done ;
  Mutex.unlock mtx
let [@inline] wait_until t mtx cond =
  wait_while t mtx (fun () -> not @@ cond ())
