let rec repeat_x (n : int) : string =
  if (n < 0) then
    "Error"
  else
  if (n = 0) then
    ""
  else
    "x" ^ (repeat_x (n - 1))

(*
    Main
*)
let test x =
  let _ = print_string ("repeat_x " ^ (string_of_int x) ^ ": ") in 
  print_endline (repeat_x x)

let _ = test 5; test 4; test 3; test 2; test 1; test 0; test (-1)