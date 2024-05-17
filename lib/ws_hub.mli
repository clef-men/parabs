module type BASE =
  Ws_hub_intf.BASE

module type S =
  Ws_hub_intf.S

module Make (Base : BASE) : S
