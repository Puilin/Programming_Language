type 'a ntree = Leaf of 'a | Node of ('a ntree list);;

let rec searchMaxChildren l m =
  match l with
    [] -> m
  | hd::tl -> match hd with
      Leaf n -> searchMaxChildren tl m
    | Node t -> let len = List.length t in (if m < len then searchMaxChildren tl len else searchMaxChildren tl m)

let findn t =
  match t with
    Leaf n -> 1
  | Node t2 -> searchMaxChildren t2 (List.length t2)