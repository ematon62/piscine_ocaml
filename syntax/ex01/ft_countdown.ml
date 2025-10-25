let ft_print_int (x : int) : unit = 
  print_int x; print_char '\n'

let rec ft_countdown (x : int) : unit = 
  if (x <= 0) then
    ft_print_int 0
  else
    let _ = ft_print_int x in
    ft_countdown (x - 1)

let main =
  print_endline " ------------------------------- Test for 10 -------------------------------";
  ft_countdown 10;
  print_endline "-------------------------------- Test for 3 --------------------------------";
  ft_countdown 3;
  print_endline "-------------------------------- Test for 0 --------------------------------";
  ft_countdown 0;
  print_endline "-------------------------------- Test for -42 ------------------------------";
  ft_countdown (-42);
  print_endline "-------------------------------- Test for -10 ------------------------------";
  ft_countdown (-10);
