type 'a ntree = Leaf of 'a | Node of ('a ntree list);;

let rec helper l r =
  match l with
    [] -> r
  | hd::tl -> match hd with
      Leaf n -> helper tl (r @ [n])
    | Node t -> helper tl (r @ (helper t []));;

let flatten t =
  match t with
    Leaf n -> [n]
  | Node l -> helper l []