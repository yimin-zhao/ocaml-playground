let duplicate list n =
  let rec dup n a =
    if n = 0 then []
    else a :: dup (n - 1) a in
  let rec aux acc = function
    | [] -> acc
    | a :: tl -> aux (acc @ (dup n a)) tl in
  aux [] list;;

let duplicate2 list n =
    let rec dup n acc x =
      if n = 0 then acc
      else dup (n - 1) (x :: acc) x in
    List.fold_left (dup n) [] (List.rev list);;

duplicate ["a"; "b"; "c"] 3;;
duplicate2 ["a"; "b"; "c"] 4;;