module type BASE =
  Ws_deques_intf.BASE

module type S =
  Ws_deques_intf.S

module Make (Base : BASE) : S
