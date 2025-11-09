let crossover (l1 : 'a list) (l2 : 'a list) : 'a list =
  let rec is_in_list (elem : 'a) (l : 'a list) : bool = match l with
    | [] -> false
    | hd::tl -> if hd = elem then true else is_in_list elem tl in
  let rec crossover_aux l_1 l_2 commons = match l_1 with
    | [] -> commons
    | hd::tl -> crossover_aux tl l_2 (if ((is_in_list hd l_2) && not (is_in_list hd commons)) then 
      commons @ [hd] 
    else commons) in crossover_aux l1 l2 []

(*
  Main
*)
let test_char l1 l2 = List.iter (Printf.printf "%c") (crossover l1 l2); print_newline ()
let test_int l1 l2= List.iter (Printf.printf "%d") (crossover l1 l2); print_newline ()

let _ = 
  test_char [] [];
  test_char ['a'] ['b';'c'];
  test_char ['b';'b';] ['b';'c'];
  test_char ['a';'b';'c';] ['a';'b';'c';];
  test_char ['a';'a';'b';] ['b';'a';'a';];
  test_char ['a';'a';'b';] [];
  test_char [] ['a';'a';'b';];
  test_int [] [];
  test_int [1] [2;3];
  test_int [2;] [3;2];
  test_int [1;2;3] [1];
  test_int [1;2;3] [1;2;3];
