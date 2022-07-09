let length list =
  let rec aux n = function
    | [] -> n
    | x :: tl -> aux (n + 1) tl in
  aux 0 list;;

length [];;
length ["a"; "b"; "c"; "d"];;
