let rec horner1 ?(acc=0.0) coeffs x  =
    match coeffs with
    | []            -> acc
    | coeff::coeffs -> horner1 ~acc:(x *. acc +. coeff) coeffs x ;;

let horner2 coeffs x =
    List.fold_left (fun res coeff -> x *. res +. coeff) 0.0 coeffs;;
    
(* f(x) = x^3 - x + 2, f(3) = ? *)
horner1 [1.; 0.; -1.; 2.] 3. 
horner2 [1.; 0.; -1.; 2.] 3. 
