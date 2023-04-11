let rec add_entry a l =
  match l with 
    [] -> []
  | hd::tl -> [a :: hd] @ (add_entry a tl);;

let rec powerset l =
  match l with
    [] -> [[]]
  | hd::tl -> (add_entry hd (powerset tl)) @ (powerset tl)