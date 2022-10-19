let slice list l r =
  let rec take n = function
    | [] -> []
    | h :: tl -> if n = 0 then [] else h :: take (n - 1) tl in
  let rec drop n = function
    | [] -> []
    | h :: tl as t -> if n = 0 then t else drop (n - 1) tl in
  take (r - l + 1) (drop l list);;

let rec fold_until f acc n = function
  | [] -> (acc, [])
  | h :: t as l ->
    if n = 0 then (acc, l)
    else fold_until f (f acc h) (n - 1) t;;

let slice2 list l r =
  let _, list = fold_until (fun _ _ -> []) [] l list in
  let taken, _ = fold_until (fun acc h -> h :: acc) [] (r - l + 1) list in
  List.rev taken;;

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
slice2 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;