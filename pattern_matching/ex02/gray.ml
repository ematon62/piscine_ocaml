
let gray (n : int) : unit = 
  let list_reverse l =
    let rec rev_aux reved l = match l with
      | [] -> reved
      | hd::tl -> rev_aux (hd::reved) tl
    in rev_aux [] l in

  let rec list_prefix s l = match l with
    | [] -> []
    | hd::tl -> (s ^ hd)::(list_prefix s tl) in

  let rec gray_aux (k : int) (s : string) : string = match k with
  | _ when k = n -> s
  | k -> let ln = String.split_on_char ' ' s in
        let first_part = list_prefix "0" ln in
        let second_part = list_prefix "1" (list_reverse ln) in
        gray_aux (k + 1) ((String.concat " " first_part) ^ " " ^ (String.concat " " second_part)) in
  
  if (n <= 0) then () else print_endline (gray_aux 1 "0 1")

(*
    Main
*)
let test n = Printf.printf "Gray code of %d:\n" n; gray n; print_newline ()

let _ =
  test (-1);
  test 0;
  test 1;
  test 2;
  test 3;
  test 4;
  test 5;