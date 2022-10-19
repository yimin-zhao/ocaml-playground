let rec duplicate = function
 | [] -> []
 | a :: tl -> a :: a :: duplicate tl;;

duplicate ["a"; "b"; "c"; "c"; "d"];;