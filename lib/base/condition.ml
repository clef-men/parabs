include Stdlib.Condition

let [@inline] wait_while t mtx pred =
  while pred () do
    wait t mtx
  done

let [@inline] wait_until t mtx pred =
  wait_while t mtx (fun () -> not @@ pred ())
