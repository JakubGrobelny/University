type 'a mtree = MNode of 'a * 'a forest
and 'a forest =
    | EmptyForest
    | Forest of 'a mtree * 'a forest

let flatten xss =
    List.fold_left (fun xs ys -> xs @ ys) [] xss

let rec df_forest forest =
    match forest with
    | EmptyForest -> []
    | Forest (tree, forest) -> df_mtree tree @ df_forest forest
and df_mtree tree =
    match tree with
    | MNode (value, forest) -> value :: df_forest forest

let rec extract_level forest =
    match forest with
    | EmptyForest -> [], []
    | Forest (MNode (v, f0), f1) ->
        let vs, fs = extract_level f1 in v :: vs, f0 :: fs

let rec bf_forest forest = 
    match forest with
    | EmptyForest -> []
    | _ -> 
    let level_vs, level_fs = extract_level forest in
        let traversed = List.map bf_forest level_fs in
        level_vs @ flatten traversed
let bf_mtree tree =
    match tree with
    | MNode (value, forest) -> value :: bf_forest forest

type 'a mtree_lst = MTree of 'a * ('a mtree_lst) list

let rec df_mtree_lst tree =
    match tree with
    | MTree (value, []) -> [value]
    | MTree (value, children) ->
        let traversed = List.map df_mtree_lst children in
        value :: flatten traversed

let rec extract_level_lst trees =
    match trees with
    | [] -> [], []
    | MTree (value, children) :: trees ->
        let level_values, level_trees = extract_level_lst trees in
        value :: level_values, children :: level_trees

let rec bf_mtree_lst tree =
    let rec aux trees =
        match trees with
        | [] -> []
        | _ -> let level_vs, level_ts = extract_level_lst trees in
            let traversed = List.map aux level_ts in
            level_vs @ flatten traversed in
    match tree with
    | MTree (value, children) -> value :: aux children

let t0 = MNode(5, EmptyForest)
let t1 = MNode(4, EmptyForest)
let t2 = MNode(6, EmptyForest)
let t3 = MNode(2, Forest(t1, Forest(t0, Forest(t2, EmptyForest))))
let t4 = MNode(3, EmptyForest)
let t5 = MNode(1, Forest(t3, Forest(t4, EmptyForest)))
    

let t0' = MTree(5, [])
let t1' = MTree(4, [])
let t2' = MTree(6, [])
let t3' = MTree(2, [t1'; t0'; t2'])
let t4' = MTree(3, [])
let t5' = MTree(1, [t3'; t4'])