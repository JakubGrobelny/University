type 'a lnode = {item: 'a; mutable next: 'a lnode};;

let make_circular_list e =
    let rec x = {item=e; next=x}
    in x

let insert_head e l =
    let x = {item=e; next=l.next}
    in l.next <- x; l

let insert_tail e l =
    let x = {item=e; next=l.next}
    in l.next <- x; x

let rec make_list n =
    match n with
    | 0 -> failwith "error"
    | 1 -> make_circular_list 1
    | n -> insert_tail n (make_list (n-1))

let rec step n l =
    match n with
    | 0 -> l
    | n -> step (n-1) l.next

let remove_head l =
    let it = l.next.item
    in l.next <- l.next.next; l, it

let josephus n m =
    let rec josephus' l acc =
        if l == l.next
            then l.item :: acc
            else let l = step (m-1) l in
                 let l', lhd = remove_head l
                    in josephus' l' (lhd :: acc)
    in List.rev (josephus' (make_list n) [])
