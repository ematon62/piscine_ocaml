let lower_offset = 97
let upper_offset = 65

let is_lower_alpha (c : char) : bool = (c >= 'a') && (c <= 'z')

let is_upper_alpha (c : char) : bool = (c >= 'A') && (c <= 'Z')

let ft_rot_1 (c : char) : char =
  if not (is_lower_alpha c || is_upper_alpha c) then
    c
  else
    if is_lower_alpha c then
      let lower_ctoi = (int_of_char c - lower_offset + 1) in (* 1 <= ctoi <= 26 *)
      char_of_int (lower_ctoi mod 26 + lower_offset)
    else
      let upper_ctoi = (int_of_char c - upper_offset + 1) in
      char_of_int (upper_ctoi mod 26 + upper_offset) 

let rec ft_rot_n (n : int) (s : string) : string =
  if (n <= 0) then
    s
  else
    ft_rot_n (n - 1) (String.map ft_rot_1 s)

(*
    Main
*)
let test n s =
  let _ = print_string ("ft_rot_n " ^ string_of_int n ^ " " ^ s ^ ": ") in
  print_endline (ft_rot_n n s)

let _ = 
  test 1 "abcdefghijklmnopqrstuvwxyz";
  test 13 "abcdefghijklmnopqrstuvwxyz";
  test 26 "abcdefghijklmnopqrstuvwxyz";
  test 2 "OI2EAS67B9";
  test 0 "Damn Daniel";
  test 1 "NBzlk qnbjr !";
  test 1000 "";
