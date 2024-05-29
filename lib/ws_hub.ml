module type BASE =
  Ws_hub_intf.BASE

module type S =
  Ws_hub_intf.S

module Make (Base : BASE) : S = struct
  include Base

  let pop_steal_until ~max_round_noyield t i pred =
    match pop t i with
    | Some _ as res ->
        res
    | None ->
        steal_until t i pred ~max_round_noyield

  let pop_steal ~max_round_noyield ~max_round_yield t i =
    match pop t i with
    | Some _ as res ->
        res
    | None ->
        steal t i ~max_round_noyield ~max_round_yield
end
