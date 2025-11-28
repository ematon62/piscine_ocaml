type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | U | None

type nucleotide = {phosphate: phosphate; deoxyribose: deoxyribose; nucleobase: nucleobase}

let generate_nucleotide (c : char) : nucleotide = 
{
  phosphate = "phosphate";
  deoxyribose = "deoxyribose";
  nucleobase = match c with | 'A' -> A | 'T' -> T | 'C' -> C | 'G' -> G | _ -> None
}

type helix = nucleotide list

let string_of_nucleobase (n : nucleobase) = match n with | A -> "A" | T -> "T" | C -> "C" | G -> "G" | U -> "U" | None -> "None"

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

type rna = nucleobase list

let rec generate_rna (h : helix) : rna = match h with
  | [] -> []
  | hd::tl -> (match hd.nucleobase with | A -> U | T -> A | G -> C | _ -> G)::(generate_rna tl)

let rec rna_to_string rna = match rna with
  | [] -> ""
  | hd::tl -> (string_of_nucleobase hd) ^ (rna_to_string tl)

let rec generate_bases_triplets (rna : rna) : (nucleobase * nucleobase * nucleobase) list = match rna with
  | h1::h2::h3::tl -> (h1, h2, h3)::(generate_bases_triplets tl)
  | _ -> []

type aminoacid =
  | Stop
  | Ala
  | Arg
  | Asn
  | Asp
  | Cys
  | Gln
  | Glu
  | Gly
  | His
  | Ile
  | Leu
  | Lys
  | Met
  | Phe
  | Pro
  | Ser
  | Thr
  | Trp
  | Tyr
  | Val

let string_of_aminoacid a = match a with
  | Stop -> "Stop"
  | Ala  -> "Ala"
  | Arg  -> "Arg"
  | Asn  -> "Asn"
  | Asp  -> "Asp"
  | Cys  -> "Cys"
  | Gln  -> "Gln"
  | Glu  -> "Glu"
  | Gly  -> "Gly"
  | His  -> "His"
  | Ile  -> "Ile"
  | Leu  -> "Leu"
  | Lys  -> "Lys"
  | Met  -> "Met"
  | Phe  -> "Phe"
  | Pro  -> "Pro"
  | Ser  -> "Ser"
  | Thr  -> "Thr"
  | Trp  -> "Trp"
  | Tyr  -> "Tyr"
  | Val  -> "Val"

type protein = aminoacid list

let rec string_of_protein (p : protein) : string = match p with
  | [] -> ""
  | hd::tl -> (string_of_aminoacid hd) ^ "-" ^ (string_of_protein tl)

(*
UAA, UAG, UGA : End of translation -> Stop
∗ GCA, GCC, GCG, GCU : Alanine -> Ala
∗ AGA, AGG, CGA, CGC, CGG, CGU : Arginine -> Arg
∗ AAC, AAU : Asparagine -> Asn
∗ GAC, GAU : Aspartique -> Asp
∗ UGC, UGU : Cysteine -> Cys
∗ CAA, CAG : Glutamine -> Gln
∗ GAA, GAG : Glutamique -> Glu
∗ GGA, GGC, GGG, GGU : Glycine -> Gly
∗ CAC, CAU : Histidine -> His
∗ AUA, AUC, AUU : Isoleucine -> Ile
∗ CUA, CUC, CUG, CUU, UUA, UUG : Leucine -> Leu
∗ AAA, AAG : Lysine -> Lys
∗ AUG : Methionine -> Met
∗ UUC, UUU : Phenylalanine -> Phe
∗ CCC, CCA, CCG, CCU : Proline -> Pro
∗ UCA, UCC, UCG, UCU, AGU, AGC : Serine -> Ser
∗ ACA, ACC, ACG, ACU : Threonine -> Thr
∗ UGG : Tryptophane -> Trp
∗ UAC, UAU : Tyrosine -> Tyr
∗ GUA, GUC, GUG, GUU : Valine -> Val
*)

let decode_arn (rna : rna) : protein =
  let rec aux (l : (nucleobase * nucleobase * nucleobase) list) : protein =
    match l with
      | (U, A, A)::(U, A, G)::(U, G, A)::tl -> Stop::[]
      | (G, C, A)::(G, C, C)::(G, C, G)::(G, C, U)::tl -> Ala::(aux tl)
      | (A, G, A)::(A, G, G)::(C, G, A)::(C, G, C)::(C, G, G)::(C, G, U)::tl -> Arg::(aux tl)
      | (A, A, C)::(A, A, U)::tl -> Asn::(aux tl)
      | (G, A, C)::(G, A, U)::tl -> Asp::(aux tl)
      | (U, G, C)::(U, G, U)::tl -> Cys::(aux tl)
      | (C, A, A)::(C, A, G)::tl -> Gln::(aux tl)
      | (G, A, A)::(G, A, G)::tl -> Glu::(aux tl)
      | (G, G, A)::(G, G, C)::(G, G, G)::(G, G, U)::tl -> Gly::(aux tl)
      | (C, A, C)::(C, A, U)::tl -> His::(aux tl)
      | (A, U, A)::(A, U, C)::(A, U, U)::tl -> Ile::(aux tl)
      | (C, U, A)::(C, U, C)::(C, U, G)::(C, U, U)::(U, U, A)::(U, U, G)::tl -> Leu::(aux tl)
      | (A, A, A)::(A, A, G)::tl -> Lys::(aux tl)
      | (A, U, G)::tl -> Met::(aux tl)
      | (U, U, C)::(U, U, U)::tl -> Phe::(aux tl)
      | (C, C, C)::(C, C, A)::(C, C, G)::(C, C, U)::tl -> Pro::(aux tl)
      | (U, C, A)::(U, C, C)::(U, C, G)::(U, C, U)::(A, G, U)::(A, G, C)::tl -> Ser::(aux tl)
      | (A, C, A)::(A, C, C)::(A, C, G)::(A, C, U)::tl -> Thr::(aux tl)
      | (U, G, G)::tl -> Trp::(aux tl)
      | (U, A, C)::(U, A, U)::tl -> Tyr::(aux tl)
      | (G, U, A)::(G, U, C)::(G, U, G)::(G, U, U)::tl -> Val::(aux tl)
      | _ -> []
  in aux (generate_bases_triplets rna)

let () = 
  let _ = Random.self_init () in
  let test_decode n = 
    let h = generate_helix n in
    let r = generate_rna h in
    let _ = Printf.printf "Printing randomly generated helix: %s\n" (helix_to_string h) in
    let _ = Printf.printf "Printing corresponding rna: %s\n" (rna_to_string r) in
    Printf.printf "Printing corresponding protein: %s\n" (string_of_protein (decode_arn r)) in
  test_decode (-1); print_newline ();
  test_decode 0; print_newline ();
  test_decode 100; print_newline ();
  test_decode 200; print_newline ();
  test_decode 300; print_newline ();
  test_decode 400; print_newline ();
  test_decode 500; print_newline ();
  test_decode 600; print_newline ();
  test_decode 700; print_newline ();