(*
  ft_alphabet_iterate (char -> unit)

  Recursively iterate over the alphabet starting from c.
  Prints the characters along the way.
  if c > 'z', infinite recursion.
*)
let rec ft_alphabet_iterate (c : char) : unit =
  let _ = print_char c in
  if (c = 'z') then
    ()
  else
    let n = int_of_char c in
    let next_char = char_of_int (n + 1) in
    ft_alphabet_iterate next_char

let ft_print_alphabet () : unit =
  let _ = ft_alphabet_iterate 'a' in
  print_char '\n'

(*
  Main
*)
let main = ft_print_alphabet ()