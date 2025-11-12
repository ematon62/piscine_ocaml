(*hfs_f and hfs_m*)

(*
	Mutually recursive functions
*)
let rec hfs_f (n : int) : int =
	if (n < 0) then
		(-1)
	else
		if (n = 0) then
			1
		else
			n - hfs_m (hfs_f (n - 1))
	and hfs_m (n : int) : int =
		if (n < 0) then
		(-1)
	else
		if (n = 0) then
			0
		else
			n - hfs_f (hfs_m (n - 1))

(*
	Main
*)
let () = 
	let test_male n = 
		Printf.printf "[Male of %d: %d] " n (hfs_m n) in
	let test_female n = 
		Printf.printf "[Female of %d: %d] " n (hfs_f n) in

	test_male (-1); test_male 0; test_male 1; test_male 2; test_male 3; test_male 4;  test_male 5; print_newline ();
	test_female (-1); test_female 0; test_female 1; test_female 2; test_female 3; test_female 4;  test_female 5; print_newline ()
