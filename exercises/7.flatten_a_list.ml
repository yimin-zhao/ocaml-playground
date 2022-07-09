type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten list =
  let rec aux res = function
    | [] -> res
    | One x :: tl -> aux (x :: res) tl
    | Many y :: tl -> aux (aux res y) tl in
  List.rev (aux [] list);;

flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
