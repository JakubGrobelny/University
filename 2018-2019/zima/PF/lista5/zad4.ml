type 'v formula =
    | True
    | False
    | Var  of 'v
    | And  of ('v formula) * ('v formula)
    | Or   of ('v formula) * ('v formula)
    | Impl of ('v formula) * ('v formula)

let filter_duplicates (pos, neg) =
    let rec aux xs =
        match xs with
        | [] -> []
        | y::ys ->
            let filtered = aux ys in
            if List.mem y filtered
                then filtered
                else y :: filtered
    in aux pos, aux neg

let extract_variables f =
    let rec extract_variables' f =
        match f with
        | True  -> [],  []
        | False -> [],  []
        | Var v -> [v], []
        | And (x, y) ->
            let x_pos, x_neg = extract_variables' x in
            let y_pos, y_neg = extract_variables' y in
            x_pos @ y_pos, x_neg @ y_neg
        | Or (x, y) ->
            let x_pos, x_neg = extract_variables' x in
            let y_pos, y_neg = extract_variables' y in
            x_pos @ y_pos, x_neg @ y_neg
        | Impl (x, y) -> 
            let x_pos, x_neg = extract_variables' x in
            let y_pos, y_neg = extract_variables' y in
            y_pos @ x_neg, y_neg @ x_pos
    in filter_duplicates (extract_variables' f)

let form1 = Impl (And (Var 'p', Impl (Var 'p', Var 'q')), Var 'q')

(* (p -> q) ∧ p -> q *)
let form2 = Impl (And (Impl (Var 'p', Var 'q'), Var 'p'), Var 'q')

type 'v proof =
    | Proof of ('v element list) * ('v formula)
and 'v element =
    | Form of ('v formula)
    | Frame of ('v formula) * ('v proof)

let rec extract_proof_variables p =
    let rec aux elements =
        match elements with
        | [] -> [], []
        | e::es ->
            let pos', neg' = aux es in
            match e with
            | Form f -> 
                let pos, neg = extract_variables f in
                pos @ pos', neg @ neg'
            | Frame (f, p) ->
                let neg, pos = extract_variables f in
                let pos'', neg'' = extract_proof_variables p in
                pos @ pos' @ pos'', neg @ neg' @ neg'' in
    match p with
    | Proof (elements, _) -> filter_duplicates (aux elements)
    
let proof1 =
    Proof (
        [Frame (
            And (Var 'p', Impl (Var 'p', Var 'q')),
            Proof (
                [Form (Var 'p'); 
                Form (Impl (Var 'p', Var 'q'))],
                Var 'q'))],
            Impl (And (Var 'p', Impl (Var 'p', Var 'q')), Var 'q'))

let proof2 = 
    Proof ([], True)

(* (p -> q) ∧ p -> q *)
let proof3 = 
    Proof([
        Frame (
            And (Impl (Var 'p', Var 'q'), Var 'p'),
            Proof(
                [Form (Var 'p'); Form (And (Impl (Var 'p', Var 'q'), Var 'p'))],
                Var 'q'
            )
        )
    ], 
    Impl (And (Impl (Var 'p', Var 'q'), Var 'p'), Var 'q'))

