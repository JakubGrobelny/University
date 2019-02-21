let rec is_palindrome xs =
    let rec aux ys snd_half =
        Printf.printf "test\n";
        match ys, snd_half with
        | _, []      -> ys, true
        | [], _      -> assert false
        | z::zs, [_] -> zs, true
        | z::zs, _::_::vs ->
            let rest, is_pal = aux zs vs in
            List.tl rest, (z = List.hd rest) && is_pal in
    match xs with
    | _ -> snd (aux xs xs)

