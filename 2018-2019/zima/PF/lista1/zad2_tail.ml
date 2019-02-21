let a n = 
  let rec a_aux n acc = 
    if n = 0 
      then acc 
      else (a_aux (n - 1) (2 * acc + 1))
  in (a_aux n 0);;

let rec print = function
  | 0 -> ()
  | n -> print_int (a n); print_string "\n"; print (n - 1);;

print 10000;;