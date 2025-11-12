let rec repeat_x (n : int) : string =
  if (n < 0) then
    "Error"
  else
  if (n = 0) then
    ""
  else
    "x" ^ (repeat_x (n - 1))

(*
    Main
*)
let () = 
  let test x =
    Printf.printf "repeat_x %d: %s\n" x (repeat_x x) in
    test 5;
    test 4;
    test 3;
    test 2;
    test 1;
    test 0;
    test (-1)
