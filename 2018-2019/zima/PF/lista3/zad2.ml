let rec calc_polynomial1 ?(xpow = 1.0) coeffs x =
    match coeffs with
    | []            -> 0.0
    | coeff::coeffs -> xpow *. coeff +. calc_polynomial1 ~xpow:(xpow *. x) coeffs x;;
    
let calc_polynomial2 coeffs x =
    List.fold_right (fun coeff res -> x *. res +. coeff) coeffs 0.0;;
        
calc_polynomial1 [1.; 0.; -1.; 2.] 3.;;
calc_polynomial2 [1.; 0.; -1.; 2.] 3.;;

