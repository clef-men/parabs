open Parabstr

module Make (Base : Ws_deques.BASE) = struct
  open Dag.Make (Ws_deques.Make (Base))

  let num_dom = 4

  let t = make (num_dom - 1)

  let rec task n () =
    if n <= 1 then (
      n
    ) else (
      let fut1 = Future.make t @@ task (n - 1) in
      let fut2 = Future.make t @@ task (n - 2) in
      Future.force t fut1 + Future.force t fut2
    )

  let _ =
    let res = ref 0 in
    run t (fun () -> res := task 20 ()) ;
    assert (!res = 6765)
end
