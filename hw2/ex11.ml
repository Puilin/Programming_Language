let rec lall l p =
  match l with
    [] -> true
  | hd::tl -> if p hd then lall tl p else false