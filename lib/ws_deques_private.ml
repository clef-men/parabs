(* FIXME: Can we relax some atomics? *)

type request =
  | Blocked
  | No_request
  | Request of int

type 'a response =
  | No_response
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
    responses= Array.init sz (fun _ -> Atomic.make No_response);
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
  | No_request ->
      if not @@ Atomic.compare_and_set request No_request Blocked then
        begin match Atomic.get request with
        | Request j ->
            block t request j
        | _ ->
            assert false
        end
  | Request j ->
      block t request j

let unblock t i =
  Atomic.set t.requests.(i) No_request ;
  t.flags.(i) <- true

let respond t i =
  let deque = t.deques.(i) in
  let request = t.requests.(i) in
  match Atomic.get request with
  | Request j ->
      let v =
        match Deque.pop_front deque with
        | Some v ->
            v
        | _ ->
            assert false
      in
      Atomic.set t.responses.(j) (Yes v) ;
      Atomic.set request (if Deque.is_empty deque then Blocked else No_request)
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
  | No_response ->
      Domain.cpu_relax () ;
      wait_response response
  | No ->
      None
  | Yes v ->
      Atomic.set response No_response ;
      Some v
let steal_to t i j =
  if t.flags.(j) && Atomic.compare_and_set t.requests.(j) No_request (Request i) then
    wait_response t.responses.(i)
  else
    None
