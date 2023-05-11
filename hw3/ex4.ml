let rec insert a l = 
  match l with
    [] -> [a]
  | hd::tl -> if hd > a then hd :: insert a tl else [a] @ l;;

let rec dsort l =
  match l with
    [] -> []
  | hd::tl -> insert hd (dsort tl)