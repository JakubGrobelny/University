type 'a list_mutable =
    | LMnil
    | LMcons of 'a * ('a list_mutable ref)

let rec list_of_mlist xs =
    match xs with
    | LMnil -> []
    | LMcons (x, xs) ->
        x :: list_of_mlist !xs 

let rec mlist_of_list xs =
    match xs with
    | [] -> LMnil
    | x::xs -> LMcons(x, ref (mlist_of_list xs))

let rec concat_copy xs ys =
    match xs with
    | LMnil -> ys
    | LMcons (x, xs) -> LMcons (x, ref (concat_copy !xs ys))

let rec concat_share xs ys =
    let rec concat_share' xs' =
        match !xs' with
        | LMnil -> xs' := ys
        | LMcons (_, xs') -> concat_share' xs'
    in match xs with
    | LMnil -> ys
    | LMcons (_, xs') -> concat_share' xs'; xs