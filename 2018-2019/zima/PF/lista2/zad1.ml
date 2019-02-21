let rec sublists xs =
    let rec map_append ?(acc=[]) elem xss =
        match xss with
        | []    -> acc
        | xs::xss -> 
            let new_acc = xs :: (elem :: xs) :: acc
                in map_append ~acc:new_acc elem xss
    in match xs with
    | []    -> [[]]
    | y::ys -> 
        let ls = sublists ys
            in map_append y ls