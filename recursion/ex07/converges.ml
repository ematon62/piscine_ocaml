let rec converges (f : 'a -> 'a) (x : 'a) (n : int) : bool = 
  if (n < 0) then
    (false)
  else
    if (n = 0) then
      false
    else
      if f x = x then
        true
      else
        converges f (f x) (n - 1)
