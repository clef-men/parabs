include Stdlib.Mutex

let [@inline] protect t fn =
  lock t ;
  Fun.protect ~finally:(fun () -> unlock t) fn
