(* Exercise 4 
   Delcare a function downto1 such that:
   downto1 f n e = f(1, f(2, ..., f(n-1, f(n, e)) 
   and the use it to define a  
   1. factorial
   2. map to a list of integers *)

let downto1 f n e = List.foldBack f [1 .. n] e

let factorial n = downto1 (fun x acc -> acc * x) n 1
let mapFunctionToList g n = downto1 (fun x acc -> (g x) :: acc) n []