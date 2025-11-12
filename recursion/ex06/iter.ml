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
let () = 
  let zero x = 0 in
  let square x = x * x in
  let double x = x * 2 in

  let test str f x n = 
    Printf.printf "Testing for %s of %d and %n -> %d\n " str x n (iter f x n) in

  test "zero" zero 1 (-1);
  test "zero" zero 1 (0);
  test "zero" zero 1 (1);
  test "square" square 2 (-1);
  test "square" square 2741233534 0;
  test "square" square 2 0;
  test "square" square 2 4;
  test "double" double 2 0;
  test "double" double 2 1;
  test "double" double 2 2;
  test "double" double 2 3;
  test "double" double 2 4;
