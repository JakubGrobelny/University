(* 1 *)
let is_square_matrix m =
    let len = List.length m in
    let row_lengths = List.map (fun x -> len = (List.length x)) m in
    List.fold_left (fun row res -> row && res) true row_lengths

(* 2 *)
let nth_column m n =
    List.map (fun row -> List.nth row (n - 1)) m

(* 3 *)
let transpose m =
    let rec (--) b e = if b > e then [] else b :: (--) (b + 1) e in
    List.map (fun n -> nth_column m n) (1 -- List.length m)

(* 4 *)
let rec zip xs ys =
    match xs, ys with
    | [], []       -> []
    | x::xs, y::ys -> (x, y) :: zip xs ys
    | _, _ -> failwith "zip: invalid arguments"

(* 5 *)
let zipf f xs ys =
    List.map (fun tuple -> f (fst tuple) (snd tuple)) (zip xs ys)

(* 6 *)
let mult_vec v m =
    let rec mult_vec_aux ?(acc=[]) v m = 
        match m with
        | []    -> acc
        | mv::m -> 
            mult_vec_aux ~acc:(List.fold_left (+.) 0. (zipf ( *. ) mv v) :: acc) v m
    in List.rev (mult_vec_aux v (transpose m))

(* 7 *)
let rec mult_matrix m1 m2 =
    match m1 with
    | []    -> []
    | v::m  -> mult_vec v m2 :: mult_matrix m m2
    