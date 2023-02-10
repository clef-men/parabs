include Stdlib.Atomic

module Counter = struct
  type nonrec t = Int.t t

  let make () =
    make 0
  let incr t =
    fetch_and_add t 1
end
