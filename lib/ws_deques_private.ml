(* FIXME: Can we relax some atomics? *)

type request =
  | Blocked
  | NoRequest
  | Request of int

type 'a response =
  | NoResponse
  | No
  | Yes of 'a

type 'a t =
  { deques: 'a Deque.t array;
    flags: bool array;
    requests: request Atomic.t array;
    responses: 'a response Atomic.t array;
  }

let create sz =
  { deques= Array.init sz (fun _ -> Deque.create ());
    flags= Array.make sz false;
    requests= Array.init sz (fun _ -> Atomic.make Blocked);
    responses= Array.init sz (fun _ -> Atomic.make NoResponse);
  }

let size t =
  Array.length t.deques

let block t request j =
  Atomic.set t.responses.(j) No ;
  Atomic.set request Blocked
let block t i =
  t.flags.(i) <- false ;
  let request = t.requests.(i) in
  match Atomic.get request with
  | Blocked ->
      ()
  | NoRequest ->
      if not @@ Atomic.compare_and_set request NoRequest Blocked then
        begin match Atomic.get request with
        | Request j ->
            block t request j
        | _ ->
            assert false
        end
  | Request j ->
      block t request j

let unblock t i =
  Atomic.set t.requests.(i) NoRequest ;
  t.flags.(i) <- true

let respond t i =
  let deque = t.deques.(i) in
  let request = t.requests.(i) in
  match Atomic.get request with
  | Request j ->
      let v =
        begin match Deque.pop_front deque with
        | Some v ->
            v
        | _ ->
            assert false
        end
      in
      Atomic.set t.responses.(j) (Yes v) ;
      Atomic.set request (if Deque.is_empty deque then Blocked else NoRequest)
  | _ ->
      ()

let push t i v =
  Deque.push_back t.deques.(i) v ;
  if t.flags.(i) then
    respond t i
  else
    unblock t i

let pop t i =
  let deque = t.deques.(i) in
  let res = Deque.pop_back deque in
  begin match res with
  | None ->
      ()
  | Some _ ->
      if Deque.is_empty deque then
        block t i
      else
        respond t i
  end ;
  res

let rec wait_response response =
  match Atomic.get response with
  | NoResponse ->
      Domain.cpu_relax () ;
      wait_response response
  | No ->
      None
  | Yes v ->
      Atomic.set response NoResponse ;
      Some v
let steal_to t i j =
  if t.flags.(j) && Atomic.compare_and_set t.requests.(j) NoRequest (Request i) then
    wait_response t.responses.(i)
  else
    None
