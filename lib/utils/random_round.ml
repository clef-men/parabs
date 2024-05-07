type t =
  { random: Random.State.t;
    array: int array;
    mutable index: int;
  }

let create sz =
  if sz < 0 then
    invalid_arg @@ __FUNCTION__ ^ ": round size must be positive" ;
  { random= Random.State.make_self_init ();
    array= Array.init sz Fun.id;
    index= sz ;
  }

let reset t =
  t.index <- Array.length t.array

let next t =
  let i = t.index in
  if i = 0 then
    invalid_arg @@ __FUNCTION__ ^ ": round is over" ;
  let arr = t.array in
  let j = Random.State.int t.random i in
  let res = arr.(j) in
  let i = i - 1 in
  arr.(j) <- arr.(i) ;
  arr.(i) <- res ;
  t.index <- i ;
  res
