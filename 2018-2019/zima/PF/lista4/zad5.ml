type 'a btree = 
    | Leaf 
    | Node of 'a btree * 'a * 'a btree

let nonzero_tree = 
    Node
        ((Node (Leaf, 2, Leaf)),
        3,
        (Node 
            (Node (Leaf, 8, Leaf),
            5,
            Leaf)))

let zero_tree = 
    Node
        ((Node (Leaf, 2, Leaf)),
        3,
        (Node 
            (Node (Leaf, 8, Leaf),
            0,
            Leaf)))

let rec prod tree =
    let rec cps_prod tree k =
        match tree with
        | Leaf -> k 1
        | Node (t0, v, t1) -> 
            cps_prod t0 (fun x -> cps_prod t1 (fun y -> k (x * v * y)))
    in cps_prod tree (fun x -> x)

let rec prod_better tree =
    let rec cps_prod tree k =
        match tree with
        | Leaf -> k 1
        | Node (t0, v, t1) ->
            if v = 0 
                then 0
                else cps_prod t0 (fun x -> cps_prod t1 (fun y -> k (x * v * y)))
    in cps_prod tree (fun x -> x)