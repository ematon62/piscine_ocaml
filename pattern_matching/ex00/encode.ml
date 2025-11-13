let encode (l: 'a list) : (int * 'a) list = 
  let rec encode_aux (l : 'a list) (t : (int * 'a)) =
    match (l, t) with
    | [],_ -> [t]
    | hd::tl, t when (snd t = hd) -> encode_aux tl (fst t + 1, snd t)
    | hd::tl,_ -> t::(encode_aux tl (1, hd))
  in match l with
  | [] -> []
  | hd::tl -> encode_aux l (0, hd)

	
let () =
	let test_char l = 
		let rec test_aux l = match l with
		| [] -> print_newline ()
		| hd::tl -> Printf.printf "%d%c " (fst hd) (snd hd); test_aux tl
	in Printf.printf "test char: "; test_aux (encode l) in
	
	let test_int l = 
		let rec test_aux l = match l with
		| [] -> print_newline ()
		| hd::tl -> Printf.printf "%d%d " (fst hd) (snd hd); test_aux tl
	in Printf.printf "test int: "; test_aux (encode l) in

	test_char ['a';'a';'a';'b';'b';'b';];
	test_char [];
	test_char ['1';'2';'3';'4'];
	test_char ['1';'1';'1';'1'];
	test_int [1;2;3;4];
	test_int [1;1;1;2;2;2;3;3;3;4;4;4;4;4;4;4;4];
	test_int []