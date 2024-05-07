module type BASE = sig
  type 'a t

  val create :
    int -> 'a t

  val size :
    'a t -> int

  val push :
    'a t -> int -> 'a -> unit

  val pop :
    'a t -> int -> 'a option

  val steal_to :
    'a t -> int -> int -> 'a option
end

module type S = sig
  include BASE

  val steal_as :
    'a t -> int -> Random_round.t -> 'a option
end

module Make (Base : BASE) : S = struct
  include Base

  let rec steal_as t sz i round n =
    if n <= 0 then (
      None
    ) else (
      let j = (i + 1 + Random_round.next round) mod sz in
      match Base.steal_to t i j with
      | None ->
          steal_as t sz i round (n - 1)
      | _ as res ->
          res
    )
  let steal_as t i round =
    let sz = Base.size t in
    steal_as t sz i round (sz - 1)
end
