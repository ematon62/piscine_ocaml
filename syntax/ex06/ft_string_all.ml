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

let test (f : (char -> bool)) (s : string) : unit = 
	let _ = print_string ("function call with string: " ^ s ^ ": ")  in
	print_endline (if ft_string_all f s then "true" else "false")

let _ =
	print_endline "------------- Tests for is_digit -------------";
	test is_digit "1abcdef1";
	test is_digit "12345";
	test is_digit "12345a ";
	test is_digit "1" ;
	test is_digit "" ;
	print_endline "------------- Tests for is_lower_alpha -------------" ;
	test is_lower_alpha "1abcdef1" ;
	test is_lower_alpha "12345" ;
	test is_lower_alpha "abcdef " ;
	test is_lower_alpha "a" ;
	test is_lower_alpha "" ;
	print_endline "------------- Tests for is_upper_alpha -------------" ;
	test is_lower_alpha "1abcdef1" ;
	test is_lower_alpha "12345" ;
	test is_lower_alpha "abcdef " ;
	test is_lower_alpha "ABCDEF " ;
	test is_lower_alpha "A" ;
	test is_lower_alpha ""