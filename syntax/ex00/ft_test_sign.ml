
let ft_test_sign x = match (x < 0) with
    | true -> print_endline "negative"
    | false -> print_endline "positive"