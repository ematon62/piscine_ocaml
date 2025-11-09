let encode (l: 'a list) : (int * 'a) list = 
  let rec encode_aux (l : 'a list) (t : (int * 'a)) (out : (int * 'a) list) =
    match (l, t) with
    | [],_ -> out @ [t]
    | hd::tl, t when (snd t = hd) -> encode_aux tl (fst t + 1, snd t) out
    | hd::tl,_ -> let with_tuple = out @ [t] in encode_aux tl (1, hd) with_tuple
  in match l with
  | [] -> []
  | hd::tl -> encode_aux l (0, hd) []

let test_char l = 
	let rec test_aux l = match l with
	| [] -> print_newline ()
	| hd::tl -> Printf.printf "%d%c " (fst hd) (snd hd); test_aux tl in
	test_aux (encode l)

let test_int l = 
	let rec test_aux l = match l with
	| [] -> print_newline ()
	| hd::tl -> Printf.printf "%d%d " (fst hd) (snd hd); test_aux tl in
	test_aux (encode l)

let () =
	test_char ['a';'a';'a';'b';'b';'b';];
	test_char [];
	test_char ['1';'2';'3';'4'];
	test_char ['1';'1';'1';'1'];
	test_int [1;2;3;4];
	test_int [1;1;1;2;2;2;3;3;3;4;4;4;4;4;4;4;4];