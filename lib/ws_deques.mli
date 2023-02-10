type id = Int.t

type error =
  | Kill
  | Fail

module type BASE = sig
  type 'a dls
  type 'a t

  val dls_make : id -> 'a dls
  val make : Int.t -> 'a t

  val push : 'a t -> id -> 'a dls -> 'a -> Unit.t

  val pop : 'a t -> id -> 'a dls -> ('a, error) Result.t

  val steal_pre : 'a t -> id -> 'a dls -> Unit.t
  val steal : 'a t -> id -> 'a dls -> id -> ('a, error) Result.t
  val steal_post : 'a t -> id -> 'a dls -> Unit.t

  val kill : 'a t -> id -> 'a dls -> Unit.t
end

module type S = sig
  type 'a t

  val make : Int.t -> 'a t

  val push : 'a t -> 'a -> Unit.t

  val pop : 'a t -> ('a, error) Result.t

  val try_steal : num_round:Int.t -> 'a t -> ('a, error) Result.t
  val steal : 'a t -> 'a Option.t

  val pop_try_steal : num_round:Int.t -> 'a t -> ('a, error) Result.t
  val pop_steal : 'a t -> 'a Option.t

  val kill : 'a t -> Unit.t
end

module Make (Base : BASE) : S
