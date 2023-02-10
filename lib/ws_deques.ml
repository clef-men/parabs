type id = Int.t

type error =
  | Kill
  | Fail

module type BASE = sig
  type 'a dls
  type 'a t

  val dls_make : id -> 'a dls
  val make : Int.t -> 'a t

  val push : 'a t -> id -> 'a dls -> 'a -> Unit.t

  val pop : 'a t -> id -> 'a dls -> ('a, error) Result.t

  val steal_pre : 'a t -> id -> 'a dls -> Unit.t
  val steal : 'a t -> id -> 'a dls -> id -> ('a, error) Result.t
  val steal_post : 'a t -> id -> 'a dls -> Unit.t

  val kill : 'a t -> id -> 'a dls -> Unit.t
end

module type S = sig
  type 'a t

  val make : Int.t -> 'a t

  val push : 'a t -> 'a -> Unit.t

  val pop : 'a t -> ('a, error) Result.t

  val try_steal : num_round:Int.t -> 'a t -> ('a, error) Result.t
  val steal : 'a t -> 'a Option.t

  val pop_try_steal : num_round:Int.t -> 'a t -> ('a, error) Result.t
  val pop_steal : 'a t -> 'a Option.t

  val kill : 'a t -> Unit.t
end

module Make (Base : BASE) : S = struct
  module Round = Random.Round

  type 'a dls = {
    id : id ;
    base : 'a Base.dls ;
    round : id Round.t ;
  }

  type 'a t = {
    dls_key : 'a dls Domain.DLS.key ;
    base : 'a Base.t ;
  }

  let make sz =
    if sz <= 0 then (
      invalid_arg @@ __FUNCTION__ ^ ": size must be strictly positive"
    ) else (
      let id_gen = Atomic.Counter.make () in
      let dls_make () =
        let id = Atomic.Counter.incr id_gen in
        { id ;
          base = Base.dls_make id ;
          round = Round.make (sz - 1) (fun i -> (id + 1 + i) mod sz) ;
        }
      in
      { dls_key = Domain.DLS.new_key dls_make ;
        base = Base.make sz ;
      }
    )

  let push t v =
    let dls = Domain.DLS.get t.dls_key in
    Base.push t.base dls.id dls.base v

  let pop t =
    let dls = Domain.DLS.get t.dls_key in
    Base.pop t.base dls.id dls.base

  let rec try_steal_round t dls =
    begin match Round.next dls.round with
    | victim_id ->
        begin match Base.steal t.base dls.id dls.base victim_id with
        | Ok task ->
            Ok task
        | Error Kill ->
            Error Kill
        | Error Fail ->
            try_steal_round t dls
        end
    | exception Invalid_argument _ ->
        Error Fail
    end
  let rec try_steal ~num_round t dls =
    if num_round <= 0 then (
      Error Fail
    ) else (
      begin match try_steal_round t dls with
      | Ok task ->
          Ok task
      | Error Kill ->
          Error Kill
      | Error Fail ->
          try_steal ~num_round:(num_round - 1) t dls
      end
    )
  let try_steal ~num_round t =
    let dls = Domain.DLS.get t.dls_key in
    Base.steal_pre t.base dls.id dls.base ;
    let res = try_steal ~num_round t dls in
    Base.steal_post t.base dls.id dls.base ;
    res

  let rec steal t dls =
    let victim_id = Round.next_reset dls.round in
    begin match Base.steal t.base dls.id dls.base victim_id with
    | Ok task ->
        Some task
    | Error Kill ->
        None
    | Error Fail ->
        steal t dls
    end
  let steal t =
    let dls = Domain.DLS.get t.dls_key in
    Base.steal_pre t.base dls.id dls.base ;
    let res = steal t dls in
    Base.steal_post t.base dls.id dls.base ;
    res

  let pop_try_steal ~num_round t =
    begin match pop t with
    | Ok task ->
        Ok task
    | Error Kill ->
        Error Kill
    | Error Fail ->
        try_steal ~num_round t
    end

  let pop_steal t =
    begin match pop t with
    | Ok task ->
        Some task
    | Error Kill ->
        None
    | Error Fail ->
        steal t
    end

  let kill t =
    let dls = Domain.DLS.get t.dls_key in
    Base.kill t.base dls.id dls.base
end
