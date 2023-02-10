open Ws_deques

type status =
  | Idle
  | Busy
type request =
  | Blocked
  | No_request
  | Request of id
type 'a response =
  | No_response
  | No_task
  | Task of 'a

type 'a dls = {
  deque : 'a Deque.t ;
}

type 'a t = {
  status : status Array.t ;
  requests : request Atomic.t Array.t ;
  responses : 'a response Atomic.t Array.t ;
  kill : Bool.t Atomic.t ;
}

let dls_make _id =
  {deque = Deque.make ()}
let make sz =
  { status = Array.make sz Idle ;
    requests = Array.init sz (fun _ -> Atomic.make No_request) ;
    responses = Array.init sz (fun _ -> Atomic.make No_response) ;
    kill = Atomic.make false ;
  }

let respond t id dls =
  let request = Array.unsafe_get t.requests id in
  begin match Atomic.get request with
  | Blocked ->
      ()
  | No_request ->
      if Deque.is_empty dls.deque then (
        Array.unsafe_set t.status id Idle
      )
  | Request thief_id ->
      Atomic.set (Array.unsafe_get t.responses thief_id)
        begin match Deque.pop_front dls.deque with
        | None ->
            Array.unsafe_set t.status id Idle ;
            No_task
        | Some task ->
            if Deque.is_empty dls.deque then (
              Array.unsafe_set t.status id Idle
            ) ;
            Task task
        end ;
      Atomic.set request No_request
  end

let block t id _dls =
  let request = Array.unsafe_get t.requests id in
  begin match Atomic.get request with
  | Blocked ->
      ()
  | No_request ->
      if not @@ Atomic.compare_and_set request No_request Blocked then (
        begin match Atomic.get request with
        | Blocked
        | No_request ->
            failwith @@ __FUNCTION__ ^ ": illegal state"
        | Request thief_id ->
            Atomic.set (Array.unsafe_get t.responses thief_id) No_task ;
            Atomic.set request Blocked
        end
      )
  | Request thief_id ->
      Atomic.set (Array.unsafe_get t.responses thief_id) No_task ;
      Atomic.set request Blocked
  end

let push t id dls task =
  Deque.push_back dls.deque task ;
  if Array.unsafe_get t.status id <> Busy then (
    Array.unsafe_set t.status id Busy
  )

let pop t id dls =
  begin match Deque.pop_back dls.deque with
  | None ->
      Error Fail
  | Some task ->
      respond t id dls ;
      Ok task
  end

let steal_pre t id dls =
  block t id dls
let steal t id _dls victim_id =
  let response = Array.unsafe_get t.responses id in
  Atomic.set response No_response ;
  if Atomic.get t.kill then (
    Error Kill
  ) else (
    if Array.unsafe_get t.status victim_id = Busy
    && Atomic.compare_and_set (Array.unsafe_get t.requests victim_id) No_request (Request id)
    then (
      let rec loop () =
        begin match Atomic.get response with
        | No_response ->
            Domain.cpu_relax () ;
            loop ()
        | No_task ->
            Error Fail
        | Task task ->
            Ok task
        end
      in
      loop ()
    ) else (
      Error Fail
    )
  )
let steal_post t id _dls =
  if not @@ Atomic.get t.kill then (
    Atomic.set (Array.unsafe_get t.requests id) No_request
  )

let kill t _id _dls =
  Atomic.set t.kill true
