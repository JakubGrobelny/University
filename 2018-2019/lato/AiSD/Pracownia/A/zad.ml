let rec print_array xs =
    for i = 0 to Array.length xs - 1 do
        Printf.printf "%d " (Array.get xs i)
    done

let min a b =
    if a < b
        then a
        else b

let max a b =
    if a > b
        then a
        else b

let abs_difference a b =
    let result = a - b in
    if result < 0
        then -result
        else result

let distance houses total a b =
    let dist = abs_difference (Array.get houses a) (Array.get houses b) in
    min dist (total - dist)

let grows houses total point target = 
    if point = Array.length houses - 1
        then true
        else 
            let dist0 = distance houses total point target
            and dist1 = distance houses total (point + 1) target in
            dist0 < dist1

let avg a b = (a + b) / 2

let rec search houses total l r target = 
    if l = r
        then distance houses total l target
        else if abs_difference l r = 1
            then let dist0 = distance houses total l target
                 and dist1 = distance houses total r target in
                 max dist0 dist1
            else let mid = avg l r in
                 if grows houses total mid target
                    then search houses total mid r target
                    else search houses total l mid target

let longest_path houses total target =
    let l = search houses total 0 target target in
    let r = search houses total target (Array.length houses - 1) target in
    max l r

let solve houses total =
    let max = ref 0 in
    for i = 0 to Array.length houses - 1 do
        let length = longest_path houses total i in
        if length > (!max)
            then max := length
            else ();
    done;
    !max

let _ =
    let read_houses n =
        let total = ref 0 in
        let houses = Array.make n 0 in
        for i = 0 to n - 1 do
            let house = read_int () in
            Array.set houses i !total;
            total := !total + house;
        done;
        houses, !total in
    let houses, total = read_houses (read_int ()) in
    Printf.printf "%d" (solve houses total)
