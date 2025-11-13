let crossover (l1 : 'a list) (l2 : 'a list) : 'a list =
  let rec is_in_list (elem : 'a) (l : 'a list) : bool = match l with
    | [] -> false
    | hd::tl -> if hd = elem then true else is_in_list elem tl in

  let rec crossover_aux l_1 l_2 = match l_1 with
    | [] -> []
    | hd::tl -> if ((is_in_list hd l_2)) then
      hd::(crossover_aux tl l2) else crossover_aux tl l2
  in crossover_aux l1 l2

(*
  Main
*)
let () = 
  let test_char l1 l2 =
  Printf.printf "test char:\nl1 = ";
  List.iter (Printf.printf "%c ") l1;
  Printf.printf "\nl2 = [";
  List.iter (Printf.printf "%c ") l2;
  Printf.printf "\nresult = ";
  List.iter (Printf.printf "%c ") (crossover l1 l2); 
  Printf.printf "\n\n"; in

  let test_int l1 l2 =
  Printf.printf "test int:\nl1 = ";
  List.iter (Printf.printf "%d ") l1;
  Printf.printf "\nl2 = ";
  List.iter (Printf.printf "%d ") l2;
  Printf.printf "\nresult = ";
  List.iter (Printf.printf "%d ") (crossover l1 l2); 
  Printf.printf "\n\n"; in

  test_char [] [];
  test_char ['a';'a';'b';] [];
  test_char [] ['a';'a';'b';];
  test_char ['a'] ['b';'c'];
  test_char ['b';'b';] ['b';'c'];
  test_char ['a';'b';'c';] ['a';'b';'c';];
  test_char ['a';'a';'b';] ['b';'a';'a';];
  test_int [] [];
  test_int [1] [2;3];
  test_int [2;] [3;2];
  test_int [1;2;2;2;2;2;2;1;3] [1;2];
  test_int [1;2;3] [1;2;3];
