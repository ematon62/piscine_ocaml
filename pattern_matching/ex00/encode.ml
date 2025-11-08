let encode (l: 'a list) : (int * 'a) list = 
  let rec encode_aux (l : 'a list) (t : (int * 'a)) (out : (int * 'a) list) =
    match (l, t) with
    | [],_ -> out @ [t]
    | hd::tl, t when (snd t = hd) -> encode_aux tl (fst t + 1, snd t) out
    | hd::tl,_ -> let fuck = out @ [t] in encode_aux tl (1, hd) fuck
  in match l with
  | [] -> []
  | hd::tl -> encode_aux l (0, hd) []

let l = encode [1;1;1;1;1;1;33;33;33]

let _ = print_int (fst (List.nth l 0)); print_int (snd (List.nth l 0)); print_endline

let _ = print_int (fst (List.nth l 1)); print_int (snd (List.nth l 1)); print_endline