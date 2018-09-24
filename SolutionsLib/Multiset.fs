namespace SolutionsLib

open System.Runtime.CompilerServices

module Multiset =
    type Multiset<'T when 'T : comparison> = 
        | Mul of 'T list
        member m.elements() = 
            match m with
            | Mul lst -> lst
        member q.Item 
            with get n = 
                let rec iter lst res =
                    match lst with
                    | [] -> res
                    | x::xs when x < n -> iter xs res
                    | x::xs when x = n -> iter xs (1 + res)
                    | x::xs            -> res
                iter (q.elements() ) 0
       
    let count element mult = 
        let rec iter (Mul lst) res = 
            match lst with
            | x::xs when x < element -> iter (Mul xs) res
            | x::xs when x = element -> iter (Mul xs) (res + 1)
            | _                      -> res
        iter mult 0

    let rec insert element (Mul lst) = 
        match lst with
        | [] -> (Mul [element])
        | x::xs when x < element -> let newTail = insert element (Mul xs)
                                    Mul (x::(newTail.elements()) ) 
        | xs                     -> Mul (element::lst) 

    let intersect (Mul lst1) (Mul lst2) =
        let rec iter lst1 lst2 = 
            match (lst1, lst2) with
            | (_, []) | ([], _) -> []
            | (x::xs, y::ys) when x = y -> x::(iter xs ys)
            | (x::xs, y::ys) when x < y -> iter xs (y::ys)
            | (x::xs, y::ys)            -> iter (x::xs) ys
        Mul (iter lst1 lst2 ) 

    let remove (Mul elems) element =
        let rec iter lst element = 
            match lst with 
            | [] -> []
            | x::xs when x = element -> xs
            | x::xs when x < element ->  x :: (iter xs element)
            | xs -> xs
        Mul ( iter elems element )

    let ofList lst = 
        let rec iter l res =
            match l with
            | [] -> res
            | x::xs -> iter xs (insert x res)
        iter lst (Mul [])
    
    type Multiset<'T when 'T : comparison> with
        static member ( + ) (mult1: Multiset<'T>, mult2: Multiset<'T>) =
            let rec iter lst res = 
                match lst with 
                | [] -> res
                | x::xs -> iter xs (insert x res)
            iter (mult2.elements() ) mult1
        static member ( - ) (mult1 : Multiset<'T>, mult2: Multiset<'T>) =
            let rec iter lst res =
                match lst with
                | [] -> res
                | x::xs -> iter xs (remove res x)
            iter (mult2.elements() ) mult1
        

            