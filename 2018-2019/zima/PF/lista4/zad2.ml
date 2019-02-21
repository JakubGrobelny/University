type 'a btree = 
    | Leaf 
    | Node of 'a btree * 'a * 'a btree

let is_balanced tree =
    let ok_diff c1 c2 =
        Pervasives.abs (c1 - c2) <= 1
    in let rec aux tree =
        match tree with
        | Leaf      -> true, 0
        | Node (t0, _, t1) -> 
        let res1, res2 = aux t0, aux t1 in
        let is_ok = fst res1 && fst res2 && ok_diff (snd res1) (snd res2) in
        let count = (snd res1) + (snd res2) + 1 in
        is_ok, count 
    in fst (aux tree)

let rec build_preorder_tree xs =
    let rec split_list xs n =
        match xs, n with
        | _, 0     -> [], xs
        | [], n    -> failwith "split_list: invalid argument!"
        | y::ys, n -> let hd, tl = split_list ys (n-1) in y::hd, tl in
    match xs with
    | []    -> Leaf
    | y::ys -> let half1, half2 = split_list ys ((List.length ys) / 2) in
        Node ((build_preorder_tree half1), y, (build_preorder_tree half2))

let balanced_tree_example = 
    Node
        ((Node (Leaf, 2, Leaf)),
        3,
        (Node 
            (Node (Leaf, 8, Leaf),
            5,
            Leaf)))

let unbalanced_tree_example = 
    Node
        ((Node (Leaf, 42, Leaf)),
        6,
        balanced_tree_example)
