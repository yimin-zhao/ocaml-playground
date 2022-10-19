let split list n =
  let rec aux acc cnt = function
    | [] -> List.rev acc, []
    | h :: tl ->
      if cnt = n then List.rev (h :: acc), tl
      else aux (h :: acc) (cnt + 1) tl in
  aux [] 1 list;;


split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
split ["a"; "b"; "c"; "d"] 5;;