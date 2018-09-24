(* Exercise 6 in Chapter 5 *)

/// Domain of a relation
let dom relation =
    Set.fold (fun values (x, y) -> Set.add x values)
             Set.empty
             relation

/// Range of a relation
let rng relation =
    Set.fold (fun values (x,y) -> Set.add y values)
             Set.empty
             relation

/// applies a relation to the element
let apply relation element = 
    Set.fold (fun result (x,y) -> if x = element then Set.add y result else result) 
             Set.empty
             relation

/// A symmetric closure of a relation
let symClosure relation =
    Set.fold (fun result (x,y) -> (Set.add (x,y) result) |> Set.add (y,x) )
             (set [])
             relation

/// Cartesian product of two sets 
let cartestianProd set1 set2 = 
    Set.fold (fun acc x -> Set.union acc (Set.map (fun y -> (x,y) ) set2) ) Set.empty set1

/// Composition of two relations
let compose relation1 relation2 =
    Set.fold (fun acc (x, y) -> Set.union acc (Set.map (fun z -> (x, z) ) (apply relation2 y) ) )
             Set.empty
             relation1

/// Computes the transitive closure of a relation
let transitiveClosure relation =
    let rec iter result = 
        if Set.union (compose relation result) result = result 
            then result 
            else iter (Set.union (compose relation result) result)
    iter relation

