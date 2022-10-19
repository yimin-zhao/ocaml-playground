let drop list n =
  let rec aux cnt = function
    | [] -> []
    | a :: tl ->
      if cnt = n then aux 1 tl
      else a :: aux (cnt + 1) tl in
  aux 1 list;;


drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;