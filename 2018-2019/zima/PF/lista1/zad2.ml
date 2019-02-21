let rec a n = 
    if n = 0
        then 0
        else 2 * a (n - 1) + 1;;

(* let rec a = function *)
  (* | 0 -> 0 *)
  (* | n -> 2 * a (n - 1) + 1;; *)

let rec print = function
  | 0 -> ()
  | n -> print_int (a n); print_string "\n"; print (n - 1);;

print 10000;;