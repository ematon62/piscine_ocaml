let ft_print_int (x : int) : unit = 
  print_int x; print_char '\n'

let rec ft_countdown (x : int) : unit = 
  if (x <= 0) then
    ft_print_int 0
  else
    let _ = ft_print_int x in
    ft_countdown (x - 1)

(*
    Main
*)
let test (x : int) = 
  let _ = print_endline ("Test for " ^ string_of_int x ^ ": ") in
  let _ = ft_countdown x in
  print_newline ()

let _ =
  test 10;
  test 3;
  test 0;
  test (-42);
  test (-10);
