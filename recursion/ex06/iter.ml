let rec iter (f : int -> int) (x : int) (n : int) : int =
  if (n < 0) then
    (-1)
  else
    if (n = 0) then
      x
    else
      iter f (f x) (n - 1)

(*
  Main
*)
let zero x = let _ = print_string (__FUNCTION__ ^ " ") in 0
let square x = let _ = print_string (__FUNCTION__ ^ " ") in x * x
let double x = let _ = print_string (__FUNCTION__ ^ " ") in x * 2

let test f x n = 
  Printf.printf "Testing for f of %d and %n -> %d\n " x n (iter f x n)

let _ =
  test zero 1 (-1);
  test zero 1 (0);
  test zero 1 (1);
  test square 2 (-1);
  test square 2741233534 0;
  test square 2 4;
  test double 2 4;
  