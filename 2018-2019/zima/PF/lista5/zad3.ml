type 'v formula =
    | True
    | False
    | Var  of 'v
    | And  of ('v formula) * ('v formula)
    | Or   of ('v formula) * ('v formula)
    | Impl of ('v formula) * ('v formula)

type 'v proof =
    | Proof of ('v element list) * ('v formula)
and 'v element =
    | Form of ('v formula)
    | Frame of ('v formula) * ('v proof)

let form1 = And (Var 'p', Impl (Var 'p', Var 'q'))
let form2 = Impl (form1, Var 'q')
let proof1 = 
    Proof (
        [Frame (
            form1, 
            Proof (
                [Form (Var 'p'); 
                Form (Impl (Var 'p', Var 'q'))],
                Var 'q'))],
        form2)

let form3 = Impl (Var 'p', Var 'q')
let form4 = And (form3, Var 'p')
let form5 = Impl (form4, Var 'q')

let proof2 =
    Proof ([Frame (form4, Proof ([Form (Impl (Var 'p', Var 'q')); Form (Var 'p')], Var 'q'))], form5)