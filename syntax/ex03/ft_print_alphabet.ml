let rec ft_alphabet_iteration (c : char) : unit =
  let _ = print_char c in
  if (c = 'z') then
    ()
  else
    let n = int_of_char c in
    let next_char = char_of_int (n + 1) in
    ft_alphabet_iteration next_char
  

let ft_print_alphabet () : unit =
  let _ = ft_alphabet_iteration 'a' in
  print_char '\n'

(*
  Main
*)
let main = ft_print_alphabet ()