let stream1 n = n;;


let hd s = s 0;;

let tl s = 
  fun n -> s (n + 1);;

let add s c =
  fun n -> s n + c;;

let map f s =
  fun n -> f (s n);;

let map2 f s1 s2 =
  fun n -> f (s1 n) (s2 n);;
  
let replace n a s =
  fun n1 -> if n1 mod n = 0
              then a
              else s n1;;

let take n s =
  fun n1 -> s (n * n1);;

let rec scan f a s = 
    fun k -> 
        if k = 0 
            then (f a (s 0)) 
            else (f ((scan f a s) (k-1)) (s k));;


let rec tabulate ?(b=0) e s =
  if b > e
    then []
    else s b :: (tabulate ~b:(b + 1) e s);;

