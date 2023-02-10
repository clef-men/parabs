include module type of Stdlib.Mutex

val locked : t -> (Unit.t -> Unit.t) -> Unit.t
