let encode (l: 'a list) : (int * 'a) list = 
  let rec encode_aux (l : 'a list) (t : (int * 'a)) =
    match (l, t) with
    | [],_ -> [t]
    | hd::tl, t when (snd t = hd) -> encode_aux tl (fst t + 1, snd t)
    | hd::tl,_ -> t::(encode_aux tl (1, hd))
  in match l with
  | [] -> []
  | hd::tl -> encode_aux l (0, hd)

  