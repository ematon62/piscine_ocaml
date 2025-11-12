let rec ackermann (m : int) (n : int) : int =
  if (m < 0 || n < 0) then
    (-1)
  else if (m = 0) then
    (n + 1)
  else if (m > 0 && n = 0) then
    ackermann (m - 1) 1
  else
    ackermann (m - 1) (ackermann m (n - 1))

(*
    Main
*)
let () = 
  let test x y =
    Printf.printf "ackermann %d %d: %d\n%!" x y (ackermann x y) in

  test (-1) 7;
  test 0 0;
  test 0 2;
  test 0 3;
  test 1 0;
  test 1 1;
  test 1 2;
  test 1 3;
  test 2 0;
  test 2 1;
  test 2 2;
  test 2 3;
  test 3 0;
  test 3 1;
  test 3 2;
  test 3 3;
  test 4 0;
  test 4 1
