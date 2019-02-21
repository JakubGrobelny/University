let is_prime n =
    if n = 2
        then true
    else let rec search i =
        if i > n / 2
            then true
            else if n mod i = 0
                then false
                else search (i + 1)
    in search 2

let is_prime_multiple_of_prime n =
    let 

let rec S sum knows =
    

