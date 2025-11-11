let pi = float_of_int 4 *. atan 1.

let f (i : int) : float = (float_of_int (-1) ** float_of_int i) /. float_of_int (2 * i + 1)

let abs_fl (x : float) : float = if x < 0. then -1. *. x else x

let leibniz_pi (delta : float) : int = 
  let rec leibniz_aux (sum : float) (n : int) = 
    let new_sum = sum +. f n in
    let gap = abs_fl (4. *. new_sum -. pi) in
    if gap <= delta then
      n
    else
      (leibniz_aux [@tailcall]) new_sum (n + 1) in
  if delta < 0. then (-1) else leibniz_aux 0. 0

(*
      Main
*)
let test delta =
  Printf.printf "For delta = %f -> n = %d\n" delta (leibniz_pi delta)

let _ =
  test 1.;
  test 0.5;
  test 0.2;
  test 0.1;
  test 0.01;
  test 0.001;
  test 0.0001;
  test 0.000005;