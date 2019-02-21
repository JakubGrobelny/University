let rec partition ?(acc=[],[]) p xs =
    match xs with
    | []    -> acc
    | y::ys -> if p y
                then partition ~acc:(fst acc, y :: snd acc) p ys
                else partition ~acc:(y :: fst acc, snd acc) p ys

let rec append xs ys =
    match xs with
    | []    -> ys
    | z::zs -> z :: append zs ys

let rec quicksort ?(acc=[]) cmp xs =
    match xs with
    | []    -> acc
    | [x]   -> [x]
    | pivot::ys ->
        let left, right = partition (fun a -> cmp pivot a) ys
            in append (quicksort cmp left) (pivot :: (quicksort cmp right))