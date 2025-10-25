let rec ft_power (x : int) (n : int) : int = 
  if (n = 0) then
    1
  else
    x * (ft_power x (n - 1))


(*
    Main
*)
let _ =
  print_endline " ------------------------------- ft_power 2 2 -------------------------------";
  print_int (ft_power 2 2); print_newline();
  print_endline " ------------------------------- ft_power 2 3 -------------------------------";
  print_int (ft_power 2 3); print_newline();
  print_endline " ------------------------------- ft_power 2 4 -------------------------------";
  print_int (ft_power 2 4); print_newline();
  print_endline " ------------------------------- ft_power 2 5 -------------------------------";
  print_int (ft_power 2 5); print_newline();
  print_endline " ------------------------------- ft_power 3 1 -------------------------------";
  print_int (ft_power 3 1); print_newline();
  print_endline " ------------------------------- ft_power 3 2 -------------------------------";
  print_int (ft_power 3 2); print_newline();
  print_endline " ------------------------------- ft_power 4 5 -------------------------------";
  print_int (ft_power 4 5); print_newline();
  print_endline " ------------------------------- ft_power 1 10 -------------------------------";
  print_int (ft_power 1 10); print_newline();
  print_endline " ------------------------------- ft_power 5 0 -------------------------------";
  print_int (ft_power 5 0); print_newline();
  print_endline " ------------------------------- ft_power 0 5 -------------------------------";
  print_int (ft_power 0 5); print_newline();