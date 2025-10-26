let rec ft_power (x : int) (n : int) : int = 
  if (n = 0) then
    1
  else
    x * (ft_power x (n - 1))


(*
    Main
*)
let test (x : int) (y : int) =
  let _ = print_endline ("ft_power " ^ string_of_int x ^ " " ^ string_of_int y ^ ": ") in
  let _ = print_int (ft_power x y) in
  print_newline ()

let _ =
  test 2 2;
  test 2 3;
  test 2 4;
  test 2 5;
  test 3 1;
  test 3 2;
  test 4 5;
  test 1 10;
  test 5 0;
  test 0 5;