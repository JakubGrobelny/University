let rec cycle xs n =
    let rec reverse ?(acc = []) xs =
        match xs with
        | []    -> acc
        | y::ys ->  reverse ~acc:(y :: acc) ys 
    in let rec split xs =
        match reverse xs with
        | []    -> failwith "Invalid input!"
        | y::ys -> y, reverse ys
    in match n with
    | 0 -> xs
    | n -> let y, ys = split xs
        in cycle (y::ys) (n-1)


let rec cycle2 xs n = 
    let rec split_n xs n =
        match n with
        | 0 -> ([], xs)
        | n -> match xs with
            | []    -> failwith "take_n: the list is too short!"
            | x::xs -> let elements, tail = split_n xs (n-1)
                in x :: elements, tail
    in let rec length xs =
        match xs with
        | [] -> 0
        | y::ys -> 1 + length ys
    in let rec append xs ys =
        match xs with
        | [] -> ys
        | z::zs -> z :: append zs ys
    in match n with
    | 0 -> xs
    | n -> let m = length xs - n
        in let head, tail = split_n xs m
        in append tail head 