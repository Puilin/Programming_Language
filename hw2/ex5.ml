let rec perm n k =
  if k = 0 then 1 else n * perm (n-1) (k-1)