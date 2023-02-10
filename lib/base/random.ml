include Stdlib.Random

module Round = struct
  type 'a t = {
    rng : State.t ;
    vals : 'a Array.t ;
    mutable idx : Int.t ;
  }

  let make sz init =
    if sz <= 0 then (
      invalid_arg @@ __FUNCTION__ ^ ": round size must be strictly positive"
    ) else (
      { rng = State.make_self_init () ;
        vals = Array.init sz init ;
        idx = sz ;
      }
    )

  let reset t =
    t.idx <- Array.length t.vals

  let next t =
    let idx = t.idx in
    if idx = 0 then (
      invalid_arg @@ __FUNCTION__ ^ ": round is over"
    ) else (
      let next_idx = State.int t.rng idx in
      let v = Array.unsafe_get t.vals next_idx in
      Array.unsafe_set t.vals next_idx @@ Array.unsafe_get t.vals (idx - 1) ;
      Array.unsafe_set t.vals (idx - 1) v ;
      t.idx <- idx - 1 ;
      v
    )
  let next_reset t =
    begin try
      next t
    with Invalid_argument _ ->
      reset t ;
      next t
    end
end
