let lotto_select n m =
  let range a b =
    let rec aux acc lo hi =
      if hi < lo then acc
      else aux (hi :: acc) lo (hi - 1) in
    if b < a then aux [] b a else aux [] a b in
  let list = range 1 m in
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: tl -> if n = 0 then (h, acc @ tl) else extract (h :: acc) (n - 1) tl in
  let extract_rand list n =
    extract [] (Random.int n) list in
  let rec aux n acc list len =
    if n = 0 then acc
    else
      let picked, rest = extract_rand list len in
      aux (n - 1) (picked :: acc) rest (len - 1)
  in
  aux n [] list m;;

lotto_select 6 49;;