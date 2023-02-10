module Make (Ws_deques : Ws_deques.S) = struct
  module Treiber = struct
    type 'a t =
      'a List.t Atomic.t

    let make () =
      Atomic.make [Obj.magic ()]

    let rec push t v =
      let old_contents = Atomic.get t in
      if old_contents = [] then (
        true
      ) else (
        let contents = v :: old_contents in
        if Atomic.compare_and_set t old_contents contents then (
          false
        ) else (
          Domain.cpu_relax () ;
          push t v
        )
      )

    let rec pop t =
      let old_contents = Atomic.get t in
      begin match old_contents with
      | [] ->
          None
      | v :: contents ->
          if Atomic.compare_and_set t old_contents contents then (
            Some v
          ) else (
            Domain.cpu_relax () ;
            pop t
          )
      end

    let rec kill t fn v =
      begin match pop t with
      | None ->
          ()
      | Some v' ->
          fn v ;
          kill t fn v'
      end
    let kill t fn =
      begin match pop t with
      | None ->
          invalid_arg @@ __FUNCTION__ ^ ": function cannot be called more than once"
      | Some v ->
          kill t fn v
      end
  end

  type vertex = {
    mutable task : Unit.t -> Unit.t ;
    ins : Int.t Atomic.t ;
    outs : vertex Treiber.t ;
  }

  type dls = {
    mutable vtx : vertex Option.t ;
  }

  type t = {
    dls_key : dls Domain.DLS.key ;
    ws_deques : vertex Ws_deques.t ;
    doms : Unit.t Domain.t Array.t ;
  }
  type dag = t

  type _ Effect.t +=
    | Yield : Unit.t Effect.t

  let rec worker dls ws_deques =
    begin match Ws_deques.pop_steal ws_deques with
    | Some vtx ->
        dls.vtx <- Some vtx ;
        Atomic.incr vtx.ins ;
        vtx.task () ;
        worker dls ws_deques
    | None ->
        ()
    end
  (* let rec master dls ws_deques = *)
  (*   begin match Ws_deques.pop ws_deques with *)
  (*   | Ok vtx -> *)
  (*       dls.vtx <- Some vtx ; *)
  (*       Atomic.incr vtx.ins ; *)
  (*       vtx.task () ; *)
  (*       master dls ws_deques *)
  (*   | Error Fail -> *)
  (*       () *)
  (*   | Error Kill -> *)
  (*       failwith @@ __FUNCTION__ ^ ": attempt to kill running DAG" *)
  (*   end *)

  let make sz =
    if sz < 0 then (
      invalid_arg @@ __FUNCTION__ ^ ": size must be positive"
    ) else (
      let dls_key = Domain.DLS.new_key (fun () -> {vtx = None}) in
      let ws_deques = Ws_deques.make (sz + 1) in
      { dls_key ;
        ws_deques ;
        doms = Array.init sz (fun _ -> Domain.spawn @@ fun () -> worker (Domain.DLS.get dls_key) ws_deques) ;
      }
    )

  let size t =
    Array.length t.doms + 1

  let yield () =
    Effect.perform Yield

  module Vertex = struct
    type t = vertex

    let self dag =
      let dls = Domain.DLS.get dag.dls_key in
      begin match dls.vtx with
      | None ->
          invalid_arg @@ __FUNCTION__ ^ ": invalid domain"
      | Some t ->
          t
      end

    let release dag t =
      if Atomic.fetch_and_add t.ins (-1) = 1 then (
        Ws_deques.push dag.ws_deques t
      )

    let make dag task =
      let ins = Atomic.make 1 in
      let outs = Treiber.make () in
      let task' () =
        begin try task () with _ -> () end ;
        Treiber.kill outs (release dag)
      in
      let rec task () =
        Effect.Deep.try_with task' () { effc =
          fun (type a) (eff : a Effect.t) : ((a, _) Effect.Deep.continuation -> _) Option.t ->
            begin match eff with
            | Yield -> Some (fun k ->
                t.task <- Effect.Deep.continue k ;
                release dag t
            )
            | _ ->
                None
            end
        }
      and t =
        {task ; ins ; outs}
      in
      t

    let link t1 t2 =
      Atomic.incr t2.ins ;
      if Treiber.push t1.outs t2 then (
        Atomic.decr t2.ins
      )
  end

  let run t task =
    Vertex.release t @@ Vertex.make t (fun () ->
      task () ;
      Ws_deques.kill t.ws_deques ;
    ) ;
    worker (Domain.DLS.get t.dls_key) t.ws_deques ;
    Array.iter Domain.join t.doms

  module Future = struct
    type 'a t = {
      vtx : vertex ;
      res : 'a Option.t ref ;
    }

    let make dag task =
      let res = ref None in
      let vtx = Vertex.make dag (fun () -> res := Some (task ())) in
      Vertex.release dag vtx ;
      {vtx ; res}

    let force dag t =
      begin match !(t.res) with
      | Some v ->
          v
      | None ->
          Vertex.link t.vtx (Vertex.self dag) ;
          yield () ;
          Option.get !(t.res)
      end
  end
end
