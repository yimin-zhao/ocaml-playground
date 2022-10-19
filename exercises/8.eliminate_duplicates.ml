let rec compress  = function
 | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
 | rest -> rest;;

compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;