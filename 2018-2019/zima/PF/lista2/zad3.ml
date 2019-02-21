(* 1. *)
let rec merge cmp xs ys =
    match xs, ys with
    | [], _          -> ys
    | x::xs, []      -> xs
    | x::xs1, y::ys1 ->
        if cmp x y
            then x :: merge cmp xs1 ys
            else y :: merge cmp xs ys1

(*  2. *)
let rec reverse ?(acc = []) xs =
    match xs with
    | []    -> acc
    | x::xs -> reverse ~acc:(x :: acc) xs

let rec merge_iter ?(acc = []) cmp xs ys =
    match xs, ys with
    | [], []         -> reverse acc
    | [], y::ys      -> merge_iter ~acc:(y::acc) cmp [] ys
    | x::xs, []      -> merge_iter ~acc:(x::acc) cmp xs []
    | x::xs1, y::ys1 ->
        if (cmp x y)
            then merge_iter ~acc:(x::acc) cmp xs1 ys
            else merge_iter ~acc:(y::acc) cmp xs ys1

(* Funkcja do testowania czasu wywołania *)
let time f = 
    let t0 = Sys.time ()
    in let result = f ()
    in let t1 = Sys.time ()
    in Printf.printf "Time: %f seconds\n" (t1 -. t0);
    t1 -. t0;;

let rec range b e =
    if b > e then [] else b :: range (b+1) e

let rec map f xs =
    match xs with
    | []    -> []
    | y::ys -> f y :: map f ys

let ls0 = map (fun x -> x * 3 + 2) (range 0 10000)
let ls1 = map (fun x -> x * 2 + x) (range 0 10010)

let test0 = time (fun () -> merge (>=) ls0 ls1)
let test1 = time (fun () -> merge_iter (>=) ls0 ls1)

(* 3. *)
let rec length xs =
    match xs with
    | [] -> 0
    | x::xs -> 1 + length xs

let rec split_n xs n =
    match n with
    | 0 -> ([], xs)
    | n -> match xs with
        | []    -> failwith "take_n: the list is too short!"
        | x::xs -> let elements, tail = split_n xs (n-1)
            in x :: elements, tail

let rec mergesort cmp xs =
    match xs with
    | []  -> []
    | [x] -> [x]
    | _ ->
        let m = length xs / 2
        in let xs, ys = split_n xs m
        in merge_iter cmp (mergesort cmp xs) (mergesort cmp ys)

(* 4. ??? *)
let rec merge_iter2 ?(acc = []) cmp xs ys =
    match xs, ys with
    | [], []         -> acc
    | [], y::ys      -> merge_iter2 ~acc:(y::acc) cmp [] ys
    | x::xs, []      -> merge_iter2 ~acc:(x::acc) cmp xs []
    | x::xs1, y::ys1 ->
        if cmp x y
            then merge_iter2 ~acc:(x::acc) cmp xs1 ys
            else merge_iter2 ~acc:(y::acc) cmp xs ys1

let rec mergesort2 cmp xs =
    match xs with
    | []  -> []
    | [x] -> [x]
    | _ ->
        let m = length xs / 2
        in let xs, ys = split_n xs m
        in let ncmp = fun a b -> not (cmp a b)
        in (merge_iter2 ncmp (mergesort2 ncmp xs) (mergesort2 ncmp ys))
