let rec int_list_to_string l = match l with
  | [] -> ""
  | hd::tl -> (string_of_int hd) ^ (int_list_to_string tl)

let sequence (n : int) : string = 
  let rec next (l : int list) = match l with
    | [] -> []
    | h1::h2::h3::tl when h1 = h2 && h2 = h3 -> 3::h1::(next tl)
    | h1::h2::tl when h1 = h2 -> 2::h1::(next tl)
    | hd::tl -> 1::hd::(next tl) in
  
  let rec aux (k : int) (l : int list) : int list = match k with
    | _ when k = n -> l
    | _ -> aux (k +1) (next l) in

  if (n <= 0) then "" else int_list_to_string (aux 1 [1])

(*
  Main
*)

let () = 
  let test n = Printf.printf "%d-th term of Conway sequence: %s\n" n (sequence n) in
  test (-1);
  test 0;
  test 1;
  test 2;
  test 3;
  test 4;
  test 5;
  test 6;
  test 7;