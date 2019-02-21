type 'var formula =
    | Var of 'var
    | Negation of 'var formula
    | Conjunction of ('var formula) * ('var formula)
    | Disjunction of ('var formula) * ('var formula)

(* phi1 = ¬(A ∧ (¬A ∨ B)) *)
let phi1 = Negation 
(Conjunction 
    ((Var "A"), 
    Disjunction 
        ((Negation 
            (Var "A")), 
        Var "B")))

(* phi2 = ¬p ∨ p *)
let phi2 =
    Disjunction
    ((Negation (Var "p")),
    (Var "p"))

let phi3 = Disjunction 
    (Conjunction 
        (Disjunction
            (Conjunction(Var "a", Var "b"), Var "c"), 
            Disjunction(Var "d", Var "e")),
            Conjunction (Var "f", Var "g"))
        
(* wartościowania: jest na liście - T, nie ma - F *)
let val1 = [Var "A"];;

let rec remove_duplicates xs =
    match xs with
    | [] -> []
    | y::ys -> if List.exists (fun x -> x = y) ys
        then remove_duplicates ys
        else y :: remove_duplicates ys

let rec extract_variables formula =
    remove_duplicates
    (match formula with
    | Var _                  -> [formula]
    | Negation phi           -> extract_variables phi
    | Conjunction (phi, psi) -> extract_variables phi @ extract_variables psi
    | Disjunction (phi, psi) -> extract_variables phi @ extract_variables psi)

let rec find_var_val variable valuation =
    match valuation with
    | [] -> false
    | Var var::xs -> 
        if variable = var 
            then true
            else find_var_val variable xs
    | _::_ -> failwith "Invalid valuation!"

let rec eval formula valuation =
    match formula with
    | Var var -> find_var_val var valuation
    | Negation phi -> not (eval phi valuation)
    | Conjunction (phi, psi) -> eval phi valuation && eval psi valuation
    | Disjunction (phi, psi) -> eval phi valuation || eval psi valuation

let rec generate_valuations variables =
    match variables with
    | []    -> [[]]
    | x::xs -> let ls = generate_valuations xs in
        List.map (fun l -> x::l) ls @ ls;;

let is_tautology formula =
    let rec aux valuations =
        match valuations with
        | [] -> true, []
        | valuation::valuations ->
            if not (eval formula valuation)
                then false, valuation
                else aux valuations
    in aux (generate_valuations (extract_variables formula))

type 'var lit =
    | Pos of 'var
    | Neg of 'var

type 'var nnf =
    | NNFVar of 'var lit
    | NNFAnd of 'var nnf * 'var nnf
    | NNFOr of 'var nnf * 'var nnf

type 'var dnf_clause = 'var lit list
type 'var cnf_clause = 'var lit list

type 'var dnf = 'var dnf_clause list
type 'var cnf = 'var cnf_clause list

let rec to_nnf formula =
    match formula with
    | Var var -> NNFVar (Pos var)
    | Conjunction (phi, psi) -> NNFAnd (to_nnf phi, to_nnf psi)
    | Disjunction (phi, psi) -> NNFOr (to_nnf phi, to_nnf psi)
    | Negation phi ->
        match phi with
        | Var var -> NNFVar (Neg var)
        | Conjunction (phi1, phi2) -> 
            NNFOr (to_nnf (Negation (phi1)), to_nnf (Negation (phi2)))
        | Disjunction (phi1, phi2) -> 
            NNFAnd (to_nnf (Negation (phi1)), to_nnf (Negation (phi2)))
        | Negation phi1 -> to_nnf phi1

let to_cnf formula =
    let rec aux formula =
        match formula with
        | NNFVar var -> [var]
        | NNFAnd (phi, psi) -> aux phi @ aux psi
        | NNFOr (phi, NNFAnd (psi1, psi2)) ->
            aux (NNFAnd (NNFOr (phi, psi1), NNFOr (phi, psi2)))
        | NNFOr (NNFAnd (phi1, phi2), psi) ->
            aux (NNFAnd (NNFOr (phi1, psi), NNFOr (phi2, psi)))
        | NNFOr (phi, psi) ->
            (aux phi @ aux psi)
    in aux (to_nnf formula)

let rec is_cnf_tautology formula =
    let rec get_clause_variables formula =
        match formula with
        | Var _ -> [formula], []
        | Negation var -> [], [var]
        | Disjunction (phi, psi) ->
            let p1, n1 = get_clause_variables phi in
            let p2, n2 = get_clause_variables psi in
            (p1 @ p2) , (n1 @ n2)
        | _ -> failwith "extract_negated_variables: invalid argument!"
        (* ^ rozpatrujemy tylko pojedyncze klauzule z formuły w CNF *)
    in let rec list_contains xs x =
        match xs with
        | [] -> false
        | y::ys -> y = x || list_contains ys x
    in let rec aux formula =
        match formula with
        | Var _ -> false
        | Negation phi -> false
        | Conjunction (phi, psi) -> aux phi && aux psi
        | Disjunction (phi, psi) ->
            let non_negated, negated = get_clause_variables formula in
            List.fold_left (fun acc v -> acc || list_contains negated v) false non_negated
    in aux (to_cnf formula)

let rec to_dnf formula =
    to_nnf (Negation (to_cnf (Negation formula)))

let rec is_dfn_unsatisfiable formula =
    is_cnf_tautology (Negation formula)