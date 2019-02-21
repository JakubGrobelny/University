let min_greater_than elem xs =
    let filtered = List.filter (fun x -> x > elem) xs in
        if filtered = []
            then elem
            else List.hd (List.sort compare filtered)

let rec reverse_sort xs = List.sort (fun lhs rhs -> compare rhs lhs) xs

let rec swap from new_val xs =
    match xs with
    | []    -> []
    | y::ys -> 
        if y = from
            then new_val :: ys
            else y :: swap from new_val ys

let next_permutation permutation =
    let rec next_perm_aux acc perm =
        match perm with
        | [] -> reverse_sort permutation
        | p::ptail -> 
            let greater = min_greater_than p acc in
            if greater == p
                then next_perm_aux (acc @ [p]) ptail
                else (reverse_sort (swap greater p acc)) @ (greater::ptail)
    in next_perm_aux [] permutation

let generate_permutations xs =
    let rec perm_aux permutations previous =
        if previous = xs
            then permutations
            else perm_aux (previous :: permutations) (next_permutation previous) 
    in perm_aux [xs] (next_permutation xs)
        