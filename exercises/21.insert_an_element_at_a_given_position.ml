let rec insert_at ele i = function
  | [] -> [ele]
  | a :: tl as l->
    if i = 0 then ele :: l
    else a :: insert_at ele (i - 1) tl;;

insert_at "alfa" 0 ["a"; "b"; "c"; "d"];;
insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
insert_at "alfa" 2 ["a"; "b"; "c"; "d"];;
insert_at "alfa" 3 ["a"; "b"; "c"; "d"];;
insert_at "alfa" 4 ["a"; "b"; "c"; "d"];;