(*
  ft_print_number (int -> unit)

  Prints x. If x is a digit, 0 is printed before x.
*)
let ft_print_number (x : int) : unit =
  if (x >= 0 && x <= 9) then
    let _ = print_int 0 in print_int x
  else
    print_int x

let ft_print_combination (i : int) (j : int) : unit = 
  ft_print_number i;
  print_char ' ';
  ft_print_number j;
    if (i = 98) then
      ()
    else
      let _ = print_char ',' in
      print_char ' '

let ft_print_comb2 () = 

  let rec ft_second_combination (i : int) (j : int) : unit =
  if (j = 100) then
    ()
  else
    let _  = ft_print_combination i j in ft_second_combination i (j + 1) in

  let rec ft_first_combination (x : int) : unit =
  if (x = 99) then
    ()
  else
    let _ = ft_second_combination x (x + 1) in ft_first_combination (x + 1) in
  
  ft_first_combination 0; print_char '\n'

let _ = ft_print_comb2 ()