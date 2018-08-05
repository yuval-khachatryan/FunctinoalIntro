(* Solution of exercise 4.12 using List.fold or List.foldBack 
   Implementation of sum (p, xs) which sums the elements of the list xs
   that satisfy the predicate p *)

let sum (p, xs) = 
    List.fold (fun acc x -> if p x then x + acc else acc) 0 xs

