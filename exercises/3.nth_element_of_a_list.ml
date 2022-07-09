let rec at n = function
  | [] -> None
  | h :: tl -> if n = 0 then Some h else at (n - 1) tl;;

at 3  ["a"; "b"; "c"; "d"];;
at 10  ["a"; "b"; "c"; "d"];;
