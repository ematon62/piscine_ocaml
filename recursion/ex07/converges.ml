let rec converges (f : 'a -> 'a) (x : 'a) (n : int) : bool = 
  if (n < 0) then
    (false)
  else
    if (n = 0) then
      f x = x
    else
      converges f (f x) (n - 1)

(*
  Main
*)
let test f x n =
  Printf.printf "For f of %d and %n, does f converge? -> %s\n" x n (if converges f x n then "yes" else "false")

let _  =
  test (( * ) 2) 2 5;
  test (fun x -> x / 2) 2 3;
  test (fun x -> x / 2) 2 2;
  test (fun x -> 0) 2 0;
  test (fun x -> x / 5 + 4) 18 2;