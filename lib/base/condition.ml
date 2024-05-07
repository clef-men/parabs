include Stdlib.Condition

let [@inline] wait_while t mtx cond =
  while cond () do
    wait t mtx
  done

let [@inline] wait_until t mtx cond =
  wait_while t mtx (fun () -> not @@ cond ())
