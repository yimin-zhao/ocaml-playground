let remove_at1 i list =
  let rec aux cnt = function
    | [] -> []
    | a :: tl ->
      if cnt = i then tl
      else a :: aux (cnt + 1) tl in
  aux 0 list;;

let rec remove_at i = function
  | [] -> []
  | a :: tl ->
    if i = 0 then tl
    else a :: remove_at (i - 1) tl;;

remove_at 0 ["a"; "b"; "c"; "d"];;
remove_at 1 ["a"; "b"; "c"; "d"];;
remove_at 2 ["a"; "b"; "c"; "d"];;
remove_at 3 ["a"; "b"; "c"; "d"];;
remove_at 4 ["a"; "b"; "c"; "d"];;