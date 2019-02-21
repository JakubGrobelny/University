let rec permutations xs =
    let rec length xs =
        match xs with
        | []    -> 0
        | y::ys -> 1 + length ys
    in let rec insert elem xs n =
        match n, xs with
        | 0, _     -> elem :: xs
        | n, []    -> failwith "insert: invalid input!"
        | n, y::ys -> y :: insert elem ys (n-1)
    in let rec range b e =
        if b > e then [] else b :: range (b+1) e
    in let rec map f xs =
        match xs with
        | []    -> []
        | y::ys -> (f y) :: map f ys
    in let rec append xs ys =
        match xs with
        | []    -> ys
        | x::xs -> x :: append xs ys
    in let rec flatten xs =
        match xs with
        | []      -> []
        | ys::yss -> append ys (flatten yss)
    in match xs with
    | []    -> [[]]
    | y::ys -> 
        let perm = permutations ys
        in match perm with
        | [] -> failwith "permutations: error"
        | permhd::permtl ->
            let len = length permhd
            in let positions = range 0 len
            in let inserted = map (fun n -> 
                                    map (fun xs -> 
                                        insert y xs n) perm) positions
            in flatten inserted