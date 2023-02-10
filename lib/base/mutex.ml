include Stdlib.Mutex

let [@inline] locked t f =
  lock t ;
  Fun.protect ~finally:(fun () -> unlock t) f
