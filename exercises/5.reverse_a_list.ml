let rev list =
  let rec aux rest = function
    | [] -> rest
    | x :: tl -> aux (x :: rest) tl in
  aux [] list;;

rev ["a"; "b"; "c"; "d"];;
