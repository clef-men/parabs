include module type of Stdlib.Condition

val wait_while :
  t -> Mutex.t -> (unit -> bool) -> unit

val wait_until :
  t -> Mutex.t -> (unit -> bool) -> unit
