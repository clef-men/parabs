open Parabstr

module Make (Base : Ws_deques.BASE) = struct
  open Domain_pool.Make (Ws_deques.Make (Base))

  let num_dom = 4

  let t = make (num_dom - 1)

  let rec task n =
    if n <= 1 then (
      n
    ) else (
      let res1 = async t @@ fun () -> task (n - 1) in
      let res2 = async t @@ fun () -> task (n - 2) in
      await res1 + await res2
    )

  let _ =
    let res = run t (fun () -> task 20) in
    kill t ;
    assert (res = 6765)
end
