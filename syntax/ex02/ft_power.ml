let rec ft_power (x : int) (n : int) : int = 
  if (n = 0) then
    1
  else
    x * (ft_power x (n - 1))


(*
    Main
*)
let test (x : int) (y : int) =
  Printf.printf "ft_power %d %d: %d\n" x y (ft_power x y)

let _ =
  test 2 2;
  test 2 3;
  test 2 4;
  test 2 5;
  test 3 1;
  test 3 2;
  test 4 5;
  test 1 10;
  test 5 0;
  test 0 5;