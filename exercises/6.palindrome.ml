let is_palindrome list =
  list = (rev list);;

is_palindrome ["a"; "b"; "c"; "d"];;
is_palindrome ["a"; "b"; "b"; "a"];;
