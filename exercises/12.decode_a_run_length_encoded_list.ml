type 'a rle = One of 'a | Many of int * 'a


let rec unpack n a l =
  if n = 0 then l
  else unpack (n - 1) a (a :: l);;

unpack 2 "a" [];;

let decode list =
  let rec aux acc = function
    | One a :: tl -> aux (a :: acc) tl
    | Many (n, a) :: tl -> aux ((unpack n a []) @ acc) tl
    | [] -> acc in
  List.rev (aux [] list);;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;