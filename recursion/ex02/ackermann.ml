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
let test x y =
  Printf.printf "ackermann %d %d: %d\n" x y (ackermann x y)

let _ = test (-1) 7; test 0 0; test 2 3; test 4 1