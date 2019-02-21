let compose f g 
    = fun x -> f (g x);;

let rec repeat n f =
  if n = 0
    then 
      fun x -> x
    else
      compose f (repeat (n - 1) f);;

let rec multiply a b = 
  repeat a ((+) b) 0;;

let (@@) a b = a * b;;

let rec (@@) a b = 
  repeat b (( * ) a) 1;;

