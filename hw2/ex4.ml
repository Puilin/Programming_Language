let rec comb n k =
  if k = 0 || k = n then 1 else comb (n-1) (k-1) + comb (n-1) k