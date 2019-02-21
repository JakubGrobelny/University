let rec join_duplicates xs =
    let rec split_by xs element =
        match xs with
        | []    -> [], []
        | y::ys -> 
            if y = element 
                then let head, tail = split_by ys element in
                    y :: head, tail
                else [], xs 
    in match xs with
        | []    -> []
        | y::ys -> let duplicates, rest = split_by xs y in
                    duplicates :: join_duplicates rest
