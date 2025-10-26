let rec ft_print_units (n1 : int) (n2 : int) (n3 : int) : unit = 
  if (n3 = 10) then
    ()
  else
    let _ = print_int n1 in
    let _ = print_int n2 in
    let _ = print_int n3 in
    let _ = (if (n1 != 7) then print_string ", " else ()) in
    ft_print_units n1 n2 (n3 + 1)

let rec ft_print_dozens (n1 : int) (n2 : int) : unit = 
  if (n2 = 9) then
    ()
  else
    let _ = ft_print_units n1 n2 (n2 + 1) in
    ft_print_dozens n1 (n2 + 1)

let rec ft_print_cents (n1 : int) : unit = 
  if (n1 = 8) then
    ()
  else
    let _ = ft_print_dozens (n1) (n1 + 1) in
    ft_print_cents(n1 + 1)

let ft_print_comb () : unit =
  let _ = ft_print_cents 0 in print_string "\n"

(*
  Main
*)
let _ = ft_print_comb ()