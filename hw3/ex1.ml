let rec rev l =
  match l with
    [] -> []
  | hd::tl -> (rev tl) @ [hd]

let rec revrev t =
  match t with
    [] -> []
  | hd::tl -> (revrev tl) @ [rev hd]