let counter = ref 0;;

let alterSum t =
  let rec helper l acc sign = match l with
      [] -> (counter := 0 ; acc)
    | hd :: tl -> if !counter = 0 then (counter := !counter + 1 ; helper tl (acc + sign * hd) sign)
        else (counter := !counter + 1 ; let newSign = -sign in helper tl (acc + sign * hd) newSign)
  in helper t 0 1;;