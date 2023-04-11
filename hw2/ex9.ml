let rec length l =
  match l with
    [] -> 0
  | hd::tl -> 1 + length tl;;

let rec ltake l n =
  if n >= length l then l
  else if n = 0 then []
  else
    match l with
      [] -> []
    | hd::tl -> hd :: ltake tl (n-1);;