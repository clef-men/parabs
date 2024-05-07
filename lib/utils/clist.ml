type 'a t =
  | Closed
  | Open
  | Cons of 'a * 'a t

let rec iter t fn =
  match t with
  | Closed | Open ->
      ()
  | Cons (v, t) ->
      fn v ;
      iter t fn
