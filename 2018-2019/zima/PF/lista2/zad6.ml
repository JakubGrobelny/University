let rec suffixes xs =
    match xs with
    | [] -> [[]]
    | x::xs -> (x::xs) :: suffixes xs

let rec map f xs =
    match xs with
    | []    -> []
    | y::ys -> f y :: map f ys

let rec prefixes xs =
    match xs with
    | []    -> [[]]
    | y::ys -> [] :: map (fun zs -> y::zs) (prefixes ys)

(* let rec prefixes xs =
    let rec reverse ?(acc = []) xs =
        match xs with
        | []    -> acc
        | x::xs -> reverse ~acc:(x :: acc) xs
    in let rec map f xs =
        match xs with
        | []    -> []
        | x::xs -> (f x) :: map f xs
    in reverse (map reverse (suffixes (reverse xs))) *)