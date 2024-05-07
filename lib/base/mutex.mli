include module type of Stdlib.Mutex

val protect :
  t -> (unit -> 'a) -> 'a
