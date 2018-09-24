namespace SolutionsLib

module Polynomial = 


    let rec simplify p1 =
        match p1 with
            | [] | [0] -> []
            | x::xs -> match  simplify xs with
                       | [] -> [x]
                       | p  -> x::p
    let printDegree degree coefficient =
        match degree with
        | 0 -> string coefficient
        | 1 -> (string coefficient) + "x"
        | _ -> (string coefficient) + "x^" + (string degree)
    type Polynomial = 
        | Pol of int list
        member p.Item
            with get m =
                let rec iter lst n = 
                    match lst, n with
                    | x::xs, 0 -> x
                    | [], _    -> 0
                    | x::xs, n when n < 0 -> 0
                    | x::xs, n -> iter xs (n-1)
                match p with
                | Pol lst -> iter lst m
        static member ( + ) (Pol p1, Pol p2) =
            let rec add pol1 pol2 = 
                match (pol1, pol2) with
                | ([], _) -> pol2
                | (_, []) -> pol1
                | (x::xs, y::ys) -> (x+y)::(add xs ys)
            Pol ( simplify (add p1 p2) )
        static member ( - ) (Pol p1, Pol p2) =
            let rec sub pol1 pol2 = 
                match (pol1, pol2) with
                | ([], [])       -> [0]
                | ([], x::xs)    -> (-x)::(sub [] xs)
                | (_, [])        -> pol1
                | (x::xs, y::ys) -> (x-y)::(sub xs ys)
            (Pol (simplify (sub p1 p2) ) )
        static member ( * ) (c, Pol p) =
            let rec mul a p =
                match p with 
                | []    -> []
                | x::xs -> (a * x) :: xs
            (Pol (simplify (mul c p) ) )
        static member ( * ) (Pol p1, Pol p2) =  
            let rec mul pol1 pol2 = 
                match pol1, pol2 with
                | Pol [], _ 
                | _, Pol []             -> Pol [0]            
                | Pol (p0::p), Pol q    -> (p0 * (Pol q) ) + (mul (Pol p) (Pol (0::q)) ) 
            match mul (Pol p1)  (Pol p2) with
                | Pol q -> Pol (simplify q)
        override q.ToString() =
            let rec printCoeffs deg (Pol p) = 
                match p with
                | [] | [0]          -> "0"
                | [a0]              -> printDegree deg a0
                | a0::a when a0 = 0 -> printCoeffs (deg + 1) (Pol a)
                | a0::a             -> (printDegree deg a0) + " + " + (printCoeffs (deg+1) (Pol a) )
            printCoeffs 0 q
    let make lst = Pol (simplify lst)