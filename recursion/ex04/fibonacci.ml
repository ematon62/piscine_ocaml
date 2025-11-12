(*
  Tail recursion:
  A function is tail recursive if the returned value
  is an expression which contains only the recursive call.

  - fibo_aux is tail-recursive
  - if n >= 0 fibonacci calls fibo_aux
  -> fibonacci is tail-recursive.

  [@tailcall] function flag raises a warning if function is not tail-recursive
*)
let fibonacci (n : int) : int =
  if (n < 0) then 
    (-1)
  else
    let rec fibo_aux (i : int) (j : int) (k : int): int =
      if (k = 0) then
        i
      else
        if (k = 1) then
          i + j
        else
          (fibo_aux [@tailcall]) (i + j) (i) (k - 1) in
  fibo_aux 0 1 n

(*
  Main
*)
let () = 
  let test n =
    Printf.printf "fibonacci %d: %d\n" n (fibonacci n) in
  
  test (-1);
  test (0);
  test (1);
  test (2);
  test (3);
  test (4);
  test (5);
  test (6);
  test (7);
  test (8);
  test (9);
  test (10);
  test (11);
