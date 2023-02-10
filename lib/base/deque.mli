type 'a t

val make : Unit.t -> 'a t

val is_empty : 'a t -> Bool.t

val push_front : 'a t -> 'a -> Unit.t
val pop_front : 'a t -> 'a Option.t

val push_back : 'a t -> 'a -> Unit.t
val pop_back : 'a t -> 'a Option.t
