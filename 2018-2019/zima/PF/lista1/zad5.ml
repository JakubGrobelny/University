let ctrue: 'a -> 'a -> 'a =
  fun x y -> x;;

let cfalse: 'a -> 'a -> 'a =
  fun x y -> y;;

let cand p q x y =
  p (q (ctrue x y) (cfalse x y)) (cfalse x y);;

let cor p q x y =
  p (ctrue x y) (q (ctrue x y) (cfalse x y));;

let cbool_of_bool =
  function
  | true  -> ctrue
  | false -> cfalse;;

let bool_of_cbool b = 
    b true false;;