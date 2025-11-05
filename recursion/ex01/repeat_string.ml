let rec repeat_string ?str:(str="x") (n : int) : string = 
  if (n < 0) then
    "Error"
  else
  if (n = 0) then
    ""
  else
    str ^ (repeat_string ~str:str (n - 1))

(*
    Main
*)
let test x =
  let _ = print_string ("repeat_string " ^ (string_of_int x) ^ ": ") in 
  print_endline (repeat_string x)

let test2 str x =
  let _ = print_string ("repeat_string " ^ str ^ " " ^ (string_of_int x) ^ ": ") in 
  print_endline (repeat_string ~str:str x)

let _ = test 5; test 4; test 3; test 2; test 1; test 0; test (-1)

let _ = test2 "Toto" 5; test2 "Toto" 4; test2 "Toto" 3; test2 "Toto" 2; test2 "Toto" 1; test2 "Toto" 0; test2 "Toto" (-1);