(* Exercise 22, Chapter 4 
   A polynomial implementation using list *)

type Polynomial = float list

let rec multiplyByScalar scalar = function
    | [] -> []
    | x::xs -> scalar * x :: multiplyByScalar scalar xs

let multiplyByX polynomial = 0.0 :: polynomial

let rec addPolynomials polynomial1 polynomial2 =
    match (polynomial1, polynomial2) with
    | ([], _) -> polynomial2
    | (_, []) -> polynomial1
    | (x::xs, y::ys) -> x + y :: addPolynomials xs ys

let rec multiplyPolynomials polynomial1 polynomial2 = 
    match polynomial1 with
    | [] -> []
    | [c] -> multiplyByScalar c polynomial2
    | x::xs -> addPolynomials (multiplyByScalar x polynomial2)
                              (multiplyByX (multiplyPolynomials xs polynomial2) )

