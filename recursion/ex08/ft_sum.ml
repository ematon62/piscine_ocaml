let ft_sum (f: int -> float) (lower: int) (upper: int) : float =
  let rec ft_sum_aux (n : float) (m : int) = 
    if (m = upper) then
      (f m) +. n
    else
      (ft_sum_aux [@tailcall]) ((f m) +. n) (m + 1) in
  if (upper < lower) then nan else (ft_sum_aux [@tailcall]) 0. lower

(*
  Main
*)
let square x = float_of_int (x * x)
let identity x = float_of_int x
let reciprocal x = 1. /. float_of_int x

(* Test cases *)
let () =
  Printf.printf "Sum of squares from 1 to 5 = %f\n" (ft_sum square 1 5);
  Printf.printf "Sum of identity from 3 to 7 = %f\n" (ft_sum identity 3 7);
  Printf.printf "Sum of reciprocals from 1 to 3 = %f\n" (ft_sum reciprocal 1 3);
  Printf.printf "Upper < Lower (5 to 2) = %f\n" (ft_sum square 5 2);

  Printf.printf "Sum of identity from 1 to 1_000_000 = %f\n"
    (ft_sum identity 1 1_000_000)