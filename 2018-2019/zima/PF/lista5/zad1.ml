type 'a llist = 
    | LNil
    | LCons of 'a * (unit -> 'a llist)

let lhd = function
    | LNil          -> failwith "lhd"
    | LCons (x, _)  -> x

let ltl = function
    | LNil -> failwith "ltl"
    | LCons (_, x) -> x ()

let rec lfrom k =
    LCons (k, function () -> lfrom (k+1))

let rec ltake n from =
    match n, from with
    | 0, _ -> []
    | _, LNil -> []
    | n, LCons (x, y) -> x :: ltake (n-1) (y ())
    
let pi_approx =
    let leibniz =
        let rec aux n =
            LCons ((-1. ** n) /. (2. *. n +. 1.), fun () -> aux (n +. 1.))
        in aux 0. 
    in let rec pi_aux n = 
        LCons (4. *. List.fold_left (+.) 0. (ltake n leibniz), fun () -> pi_aux (n+1))
    in pi_aux 1

let rec triple_map f stream =
    let x1 = lhd stream in
    let x2 = lhd (ltl stream) in
    let x3 = lhd (ltl (ltl stream)) in
    LCons (f x1 x2 x3, fun () -> triple_map f (ltl stream))

let pi_approx2 =
    let euler_trans x y z = z -. ((y -. z) ** 2.) /. (x -. 2. *. y +. z) in
    triple_map euler_trans pi_approx

(* **************************************************************************************8 *)

type 'a stream =
    | SNil
    | SCons of 'a * (('a stream) lazy_t)

let shd = function
    | SNil -> failwith "shd"
    | SCons (x, _) -> x

let stl = function
    | SNil -> failwith "stl"
    | SCons (_, xs) -> Lazy.force xs

let rec sfrom k =
    SCons (k, lazy (sfrom (k + 1)))

let rec stake n from =
    match n, from with
    | 0, _    ->  []
    | _, SNil -> []
    | n, SCons (x, xs) -> x :: stake (n-1) (Lazy.force xs)

let stream_pi_approx = 
    let leibniz =
        let rec aux n =
            SCons ((-1. ** n) /. (2. *. n +. 1.), lazy (aux (n +. 1.)))
        in aux 0.
    in let rec pi_aux n =
        SCons (4. *. List.fold_left (+.) 0. (stake n leibniz), lazy (pi_aux (n+1))) in
    pi_aux 1

let rec s_triple_map f stream = 
    let x1 = shd stream in
    let x2 = shd (stl stream) in
    let x3 = shd (stl (stl stream)) in
    SCons (f x1 x2 x3, lazy (s_triple_map f (stl stream)))

let stream_pi_approx2 =
    let euler_trans x y z = z -. ((y -. z) ** 2.) /. (x -. 2. *. y +. z) in
    s_triple_map euler_trans stream_pi_approx