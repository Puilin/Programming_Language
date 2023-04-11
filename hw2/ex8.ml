type btree = Leaf | Node of int * btree * btree;;
let rec prod_tree t =
  match t with
    Leaf -> 1
  | Node (n, t1, t2) -> n * (prod_tree t1) * (prod_tree t2)