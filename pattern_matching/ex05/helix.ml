type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = {phosphate: phosphate; deoxyribose: deoxyribose; nucleobase: nucleobase}

let generate_nucleotide (c : char) : nucleotide = 
{
  phosphate = "phosphate";
  deoxyribose = "deoxyribose";
  nucleobase = match c with | 'A' -> A | 'T' -> T | 'C' -> C | 'G' -> G | _ -> None
}

type helix = nucleotide list

let string_of_nucleobase (n : nucleobase) = match n with | A -> "A" | T -> "T" | C -> "C" | G -> "G" | None -> "None"

let rec generate_helix (n : int) : helix = match n with
  | _ when n <= 0 -> []
  | _ -> let random_c = (match (Random.int 3) with | 0 -> 'A' | 1 -> 'T' | 2 -> 'C' | _ -> 'G') in
          (generate_nucleotide random_c)::(generate_helix (n - 1))

let rec helix_to_string (h : helix) : string = 
  match h with
  | [] -> ""
  | hd::tl -> (string_of_nucleobase hd.nucleobase) ^ (helix_to_string tl)

let rec complementary_helix (h : helix) : helix = match h with
  | [] -> []
  | hd::tl -> (generate_nucleotide (match hd.nucleobase with | A -> 'T' | T -> 'A' | G -> 'C' | _ -> 'G'))::(complementary_helix tl)

let () = 
  let _ = Random.self_init () in
  let test_helix_string n = 
    let h = generate_helix n in
    let _ = Printf.printf "Printing randomly generated helix: %s\n" (helix_to_string h) in
    Printf.printf "     Printing complementary helix: %s\n" (helix_to_string (complementary_helix h)) in
  test_helix_string (-1); print_newline ();
  test_helix_string 0; print_newline ();
  test_helix_string 1; print_newline ();
  test_helix_string 2; print_newline ();
  test_helix_string 3; print_newline ();
  test_helix_string 4; print_newline ();
  test_helix_string 5; print_newline ();
  test_helix_string 6; print_newline ();
  test_helix_string 7; print_newline ();