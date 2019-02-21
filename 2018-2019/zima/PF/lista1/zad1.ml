(* int -> int *)
fun x -> x + 0;;

(* 'a -> 'b *)
let rec f x = f x;;

(* (’a -> ’b) -> (’c -> ’a) -> ’c -> ’b. *)
fun f g x -> f (g x);;

