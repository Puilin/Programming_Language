let rec cartesian a b =
  match a, b with
    [], _ -> []
  | _, [] -> []
  | h1::t1, h2::t2 -> (h1, h2) :: (cartesian [h1] t2) @ (cartesian t1 b)