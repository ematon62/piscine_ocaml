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
let () = 
  let test f x n =
    Printf.printf "For f of %d and %n, does f converge? -> %s\n" x n (if converges f x n then "true" else "false") in

  (* un+1 = un * 2, u0 = 2 -> admet pour suite +inf *)
  test (( * ) 2) 2 5;

  (* un+1 = un / 2, u0 = 2-> Pour tout N >= 2, un = 0 qui est sa limite. *)
  test (fun x -> x / 2) 2 0;
  test (fun x -> x / 2) 2 1;
  test (fun x -> x / 2) 2 2;
  test (fun x -> x / 2) 2 3;

  (* un+1 = un / 2, u0 = 2 -> Pour tout N >= 1, un = 0 qui est sa limite. *)
  test (fun x -> 0) 2 0;
  test (fun x -> 0) 2 1;

  (* un+1 = un / 5 + 4, u0 = 18 -> Pour tout N >= 2, un = 4 qui est sa limite. *)
  test (fun x -> x / 5 + 4) 18 0;
  test (fun x -> x / 5 + 4) 18 1;
  test (fun x -> x / 5 + 4) 18 2;
  test (fun x -> x / 5 + 4) 18 3;
  test (fun x -> x / 5 + 4) 18 4;
