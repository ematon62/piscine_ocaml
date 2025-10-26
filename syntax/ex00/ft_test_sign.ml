let ft_test_sign (x : int) : unit = 
  if (x < 0) then 
    print_endline "negative" 
  else 
    print_endline "positive"

(*
    Main
*)
let test (x : int) : unit = 
  print_string ("Sign of " ^ (string_of_int x) ^ "? -> "); ft_test_sign x

let main =
  test 0;
  test 1;
  test (-1)