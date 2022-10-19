type 'a rle =
  | One of 'a
  | Many of int * 'a


let create_tuple cnt ele =
    if cnt = 1 then One ele
    else Many (cnt, ele);;

let encode list =
  let rec aux cnt acc = function
    | [] -> []
    | [x] -> (create_tuple (cnt + 1) x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (cnt + 1) acc t
      else aux 0 ((create_tuple (cnt + 1) a) :: acc) t in
  aux 0 [] list;;

let encode1 list =
  let pack list =
    let rec aux current acc = function
      | [] -> []
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
        if a = b then aux (a :: current) acc t
        else aux [] ((a :: current) :: acc) t in
    List.rev (aux [] [] list) in
  List.map (fun l -> (create_tuple (List.length l) (List.hd l))) (pack list);;


encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
encode1 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;