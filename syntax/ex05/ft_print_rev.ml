(*
	A Ocaml program is separated into one or more modules.
	Modules are files containing type, function... definitions.
	Here, we use String.get s i (= s.[i]) and String.length from the String module.
*)

let rec ft_print_reverse_char (s : string) (i : int) : unit =
	if (i = (-1)) then
		()
	else
		let _ = print_char s.[i] in
		ft_print_reverse_char s (i - 1)

let ft_print_rev (s : string) : unit = 
	let _ = ft_print_reverse_char s (String.length s - 1) in
	print_char '\n'

(*
		Main
*)
let test s =
	let _ = print_string (s ^ " reversed : ") in ft_print_rev s

let main =
	test "Hello World!";
	test "abcdefghijklmnopqrstuvwxyz";
	test "";
	test "a";