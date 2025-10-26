(*
		Predicate function:
		functions that return a single true or false.
		You use predicate functions to check if your input meets some condition.
*)


let ft_string_all (f : (char -> bool)) (s : string) : bool =
	let rec ft_check_string (f : (char -> bool)) (s : string) (i : int) : bool = 
		if (i = String.length s) then
			true
		else
			if not (f s.[i]) then
				false
			else
				ft_check_string f s (i + 1) in
	ft_check_string f s 0

(*
				Main
*)
let is_digit (c : char) : bool = (c >= '0') && (c <= '9')

let is_lower_alpha (c : char) : bool = (c >= 'a') && (c <= 'z')

let is_upper_alpha (c : char) : bool = (c >= 'A') && (c <= 'Z')

let _ =
	let _ = print_string "ft_string_all is_digit \"abcdef\": " in
	print_endline (if ft_string_all is_digit "abcdef1" then "true" else "false")
	
let _ = 
	let _ = print_string "ft_string_all is_digit \"12456\": " in
	print_endline (if ft_string_all is_digit "12345" then "true" else "false")

let _ = 
	let _ = print_string "ft_string_all is_lower_alpha \"abcdef\": " in
	print_endline (if ft_string_all is_lower_alpha "abcdef" then "true" else "false")

let _ = 
	let _ = print_string "ft_string_all is_lower_alpha \"ABCDEF\": " in
	print_endline (if ft_string_all is_lower_alpha "ABCDEF" then "true" else "false")

let _ = 
	let _ = print_string "ft_string_all is_upper_alpha \"ABCDEF\": " in
	print_endline (if ft_string_all is_upper_alpha "ABCDEF" then "true" else "false")

let _ = 
	let _ = print_string "ft_string_all is_upper_alpha \"abcdef\": " in
	print_endline (if ft_string_all is_upper_alpha "abcdef" then "true" else "false")

let _ = 
	let _ = print_string "ft_string_all is_digit \"\": " in
	print_endline (if ft_string_all is_digit "" then "true" else "false")
