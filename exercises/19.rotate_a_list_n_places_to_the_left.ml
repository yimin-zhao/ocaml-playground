let rotate list n =
  let rec aux acc cnt = function
    | [] -> List.rev acc
    | h :: tl as l ->
      if cnt = n then l @ (List.rev acc)
      else aux (h :: acc) (cnt + 1) tl in
  aux [] 0 list;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;