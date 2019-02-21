type ('a, 'b) memo =
    | Memo of ('a * 'b) list

let init_array = ref (Memo [])

let add_result arg res results =
    match !results with
    | Memo xs ->
        results := Memo ((arg, res) :: xs)

let find_result arg results =
    let rec find_result' arg results =
        match results with
        | [] -> None
        | (a, r) :: results ->
            if a = arg
                then Some r
                else find_result' arg results
    in match !results with
    | Memo results -> find_result' arg results

let fib_memo_call_table = init_array

let rec fib_memo n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | n ->
        match find_result n fib_memo_call_table with
        | None ->
            let res = fib_memo (n-1) + fib_memo (n-2) in
            add_result n res fib_memo_call_table;
            res
        | Some res -> res

let rec fib n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | n -> fib (n-1) + fib (n-2)