let is_palindrome list =
  list = (List.rev list);;

is_palindrome ["a"; "b"; "c"; "d"];;
is_palindrome ["a"; "b"; "b"; "a"];;
