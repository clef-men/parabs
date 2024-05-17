module type BASE =
  Ws_deques_intf.BASE

module type S =
  Ws_deques_intf.S

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
