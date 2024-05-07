type 'a t

val create :
  unit -> 'a t

val push :
  'a t -> 'a -> bool

val pop :
  'a t -> 'a Option2.t

val close :
  'a t -> 'a Clist.t
