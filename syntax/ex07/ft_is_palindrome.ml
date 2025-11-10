(*
  ft_check_ends (string -> int -> int -> bool)
  
  Recursively checks if the "mirror" index elements of s are equal.
  Starts by comparing at indexes i and j.
*)
let rec ft_check_ends (s : string) (i : int) (j : int) : bool = 
  if (j <= i) then 
    true
  else 
    if (s.[i] != s.[j]) then 
      false
    else 
      ft_check_ends s (i + 1) (j - 1)

let ft_is_palindrome (s : string) : bool =
  ft_check_ends s 0 (String.length s - 1)

(*
  Main
*)
let test s = 
  Printf.printf "%s is palindrome? -> " s;
  print_endline (if ft_is_palindrome s then "yes" else "no")

let _ = 
  let _ = print_endline "------------------- ft_is_palindrome tests -------------------" in
  test "radar";
  test "madam";
  test "car";
  test "";
  test "Madam";
  test "t";
  test "tt";
  test "golem"