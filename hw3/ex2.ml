let rec union t1 t2 =
  match t1, t2 with
    [], [] -> []
  | [], _ -> t2
  | _, [] -> t1
  | hd1::tl1, hd2::tl2 -> if hd1 = hd2 then hd1 :: (union tl1 tl2) else hd1 :: (union tl1 t2)