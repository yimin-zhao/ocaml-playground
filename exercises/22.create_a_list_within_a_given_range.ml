let range l r =
  let rec aux a b =
    if a > b then [] else a :: aux (a + 1) b in
  if l > r then List.rev (aux r l) else aux l r;;


let range2 a b =
  let rec aux acc hi lo =
    if hi >= lo then
      aux (hi :: acc) (hi - 1) lo
    else acc in
  if a < b then aux [] b a else List.rev (aux [] a b);;

range2 4 9;;
range2 0 9;;
range2 13 9;;