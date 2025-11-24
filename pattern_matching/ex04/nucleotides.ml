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

(*
  Main
*)
let string_of_nucleobase (n : nucleobase) = match n with | A -> "A" | T -> "T" | C -> "C" | G -> "G" | None -> "None"

let () = 
  let test c =
    let n = generate_nucleotide c in
    Printf.printf "generate_nucleotide %c -> {phosphate = %s; deoxyribose = %s; nucleobase = %s}\n"
      c n.phosphate n.deoxyribose (string_of_nucleobase n.nucleobase) in
  test 'A'; test 'T'; test 'C'; test 'G'; test 'a'; test '1'
