(*
  tak(tak(x − 1, y, z), tak(y − 1, z, x), tak(z − 1, x, y))
*)
let rec tak (x : int) (y : int) (z : int): int =
  if (y < x) then
    tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
  else
    z
  
let test x y z =
  Printf.printf "tak %d %d %d: %d\n" x y z (tak x y z)

let _ = test 1 2 3; test 5 23 7; test 9 1 0; test 1 1 1; test 0 42 0; test 23498 98734 98776;