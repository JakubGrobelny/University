let (fresh, reset) =
    let counter = ref 0 in
    (fun s -> 
        let res = s ^ string_of_int !counter in
        incr counter; res),
    (fun n -> counter := n)
