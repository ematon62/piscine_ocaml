let ft_print_combination (n1 : int) (n2 : int) (n3 : int) : unit = 
  print_int n1;
  print_int n2;
  print_int n3;
  if (n1 != 7) then 
    print_string ", "
  else 
    ()

    
let ft_print_comb () : unit =
  let rec ft_units (n1 : int) (n2 : int) (n3 : int) : unit = 
    if (n3 = 10) then
      ()
    else
      let _ = ft_print_combination n1 n2 n3 in
      ft_units n1 n2 (n3 + 1) in
  
  let rec ft_tens (n1 : int) (n2 : int) : unit = 
    if (n2 = 9) then
      ()
    else
      let _ = ft_units n1 n2 (n2 + 1) in
      ft_tens n1 (n2 + 1) in
  
  let rec ft_hundreds (n1 : int) : unit = 
    if (n1 = 8) then
      ()
    else
      let _ = ft_tens (n1) (n1 + 1) in
      ft_hundreds(n1 + 1) in
  ft_hundreds 0; print_string "\n"

(*
  Main
*)
let _ = ft_print_comb ()