include module type of Stdlib.Condition

val wait_while : t -> Mutex.t -> (Unit.t -> Bool.t) -> Unit.t
val wait_until : t -> Mutex.t -> (Unit.t -> Bool.t) -> Unit.t
