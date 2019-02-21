open Syntax

type 'a result =
    | Error of string
    | Result of 'a prop

let validate_proof_t proof goal =
    let rec was_assumed formula assumptions = 
        List.mem formula assumptions in
    let rec validate proof assumptions = 
        match proof with
        | Ax form -> 
            if was_assumed form assumptions
                then Result form
                else Error "Ax: unproven formula!"
        | DisjIL (proof, conclusion) -> 
            (match validate proof assumptions with
            | Error e as err -> err
            | Result res -> Result (Disj (res, conclusion)))
        | DisjIR (conclusion, proof) ->
            (match validate proof assumptions with
            | Error e as err -> err
            | Result res -> Result (Disj (conclusion, res)))
        | ConjI (l, r) -> 
            let l' = validate l assumptions
            and r' = validate r assumptions in
            (match l', r' with
            | Error e, _ -> Error e
            | _, Error e -> Error e
            | Result l'', Result r'' -> Result (Conj (l'', r'')))
        | ImplI (assumption, proof) ->
            (match validate proof (assumption :: assumptions) with
            | Error e as err -> err
            | Result res -> Result (Impl (assumption, res)))
        | BotE form -> 
            if List.mem Bot assumptions
                then Result form
                else Error "BotE: false hasn't been assumed!"
        | TopI -> Result Top
        | ConjEL proof ->
            (match validate proof assumptions with
            | Error e as err -> err
            | Result res ->
                (match res with
                | Conj (l, _) -> Result l
                | _ -> Error "ConjEL: not a conjunction!"))
        | ConjER proof ->
            (match validate proof assumptions with
            | Error e as err -> err
            | Result res ->
                (match res with
                | Conj (_, r) -> Result r
                | _ -> Error "ConjER: not a conjunction!"))
        | ImplE (proofl, proofr) -> 
            let resl = validate proofl assumptions
            and resr = validate proofr assumptions in
            (match resl, resr with
            | Error e, _ -> Error e
            | _, Error e -> Error e
            | Result resl, Result resr ->
                (match resr with
                | Impl (l, r) ->
                    if l = resl
                        then Result r
                        else Error "ImplE: A != P in P -> Q"
                | _ -> Error "ImplE: not an implication!"))
        | DisjE (l, (ass1, proof1), (ass2, proof2)) ->
            let resl = validate l assumptions
            and resf1 = validate proof1 (ass1 :: assumptions)
            and resf2 = validate proof2 (ass2 :: assumptions) in
            (match resl, resf1, resf2 with
            | Error e, _, _ -> Error e
            | _, Error e, _ -> Error e
            | _, _, Error e -> Error e
            | Result resl, Result resf1, Result resf2 ->
                if resl = Disj (ass1, ass2)
                    then if resf1 = resf2
                        then Result resf1
                        else Error "DisjE: conclusions don't match!"
                    else Error "DisjE: wrong assumptions or left side proof!")
    in match validate proof [] with
    | Error e -> e
    | Result form -> 
        if form = goal
            then "Correct proof!"
            else "Proof does not prove the goal!"

let validate_proof_s proof goal =
    let rec derivable formula facts = 
        let rec conj_elimination formula facts =
            match facts with
            | [] -> false
            | Conj(l, r) :: facts ->
                l = formula || r = formula || conj_elimination formula facts
            | _::facts -> conj_elimination formula facts
        in let rec impl_elimination formula facts =
            let rec exists_impl formula facts' =
                match facts' with
                | [] -> false
                | Impl(l, r) :: facts' ->
                    if r = formula
                        then (if List.mem l facts
                            then true
                            else exists_impl formula facts')
                        else exists_impl formula facts'
                | _::facts -> exists_impl formula facts
            in exists_impl formula facts
        in let rec disj_elimination formula facts =
            let rec find_matching alternatives lhss =
                List.filter 
                    (function 
                    | Disj(l,r) -> List.mem l lhss && List.mem r lhss
                    | _ -> false)
                    alternatives
            in let implications = 
                List.filter 
                    (function | Impl(_,r) -> r = formula  
                              | _ -> false) 
                    facts
            and alternatives =
                List.filter 
                    (function | Disj(_,_) -> true 
                              | _ -> false) 
                    facts
            in let implied_from =
                List.map
                    (function | Impl(l, _) -> l 
                              | _ -> failwith "error")
                    implications
            in find_matching alternatives implied_from != []
        in if List.mem formula facts
            then true
            else (if (match formula with
            (* reguły wprowadzania *)
            | Top -> true
            | Bot -> false
            | Var p -> false
            | Conj (l, r) -> List.mem l facts && List.mem r facts
            | Disj (l, r) -> List.mem l facts || List.mem r facts
            | Impl (l, r) -> false)
                then true
                else
                (* reguły eliminacji *)
                (conj_elimination formula facts ||
                impl_elimination formula facts || 
                disj_elimination formula facts ||
                List.mem Bot facts))
    in let rec validate proof facts =
        match proof with
        | PDone form -> 
            if derivable form facts
                then Result form
                else Error "PDone: final conclusion cannot be derived!"
        | PConc (form, proof) ->
            if derivable form facts
                then validate proof (form::facts)
                else Error "PConc: formula cannot be derived!"
        | PHyp ((assumption, frame_proof), proof) ->
            let frame_res = validate frame_proof (assumption::facts)
            in match frame_res with
            | Error _ -> frame_res
            | Result res ->
                let impl = Impl (assumption, res)
                in validate proof (res :: impl :: facts)
    in match validate proof [] with
    | Error e -> e
    | Result form ->
        if form = goal
            then "Correct proof!"
            else "Proof does not prove the goal!"


let rec check_proofs proofs = 
    match proofs with
    | [] -> []
    | proof::proofs ->
        match proof with
        | TGoal (s, goal, proof) ->
            (s, validate_proof_t proof goal) :: (check_proofs proofs)
        | SGoal (s, goal, proof) ->
            (s, validate_proof_s proof goal) :: (check_proofs proofs)

let rec print_proof_validation_results results =
    match results with
    | [] -> []
    | (goal, result) :: results ->
        Printf.printf "%s -- %s\n" goal result;
        print_proof_validation_results results

(* ************************************************************************** *)

let _ =
let lexbuf = Lexing.from_channel stdin in
let proofs = Parser.file Lexer.token lexbuf in
(* powyższe wiersze wczytują listę dowodów ze standardowego wejścia
   i parsują ją do postaci zdefiniowanej w module Syntax *)
    print_int (List.length proofs); print_newline ();
    let checked_proofs = check_proofs proofs in
        print_proof_validation_results checked_proofs