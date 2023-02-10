type 'a t = {
  size : Int.t ;
  front : 'a List.t ;
  back : 'a List.t ;
}

let empty =
  {size = 0 ; front = [] ; back = []}

let [@inline] size t =
  t.size
let [@inline] is_empty t =
  size t = 0

let push t v =
  {size = t.size + 1 ; front = t.front ; back = v :: t.back}

let pop t =
  begin match t.front with
  | [] ->
      begin match List.rev t.back with
      | [] ->
          None
      | x :: xs ->
          Some (x, {size = t.size - 1 ; front = xs ; back = []})
      end
  | x :: xs ->
      Some (x, {size = t.size - 1 ; front = xs ; back = t.back})
  end
