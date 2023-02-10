type 'a t

val empty : 'a t

val size : 'a t -> Int.t
val is_empty : 'a t -> Bool.t

val push : 'a t -> 'a -> 'a t
val pop : 'a t -> ('a * 'a t) Option.t
