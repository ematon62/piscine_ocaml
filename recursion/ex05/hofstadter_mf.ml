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

let test n = 
	print_string ("Male of " ^ string_of_int n ^ ": " ^ string_of_int (hfs_m n) ^ "\n");
	print_string ("Female of " ^ string_of_int n ^ ": " ^ string_of_int (hfs_f n) ^ "\n")

let _ =
	test (-1);
	test (0);
	test 4;
	test 10;