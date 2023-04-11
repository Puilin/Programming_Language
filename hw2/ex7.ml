type btree = Leaf | Node of int * btree * btree;;
let rec sum_tree t =
  match t with
    Leaf -> 0
  | Node (n, t1, t2) -> n + (sum_tree t1) + (sum_tree t2)