(* this a naive recursive sollution not using any memorization techniques *)
/// Finds all subsets of a size k of the set {1, ... , n}

let rec allSubsets n k = 
    match (n, k) with
    | (a, _) when a < 0 -> set []
    | (_, b) when b < 0 -> set []
    | (a, b) when a < b -> set []
    | (_, b) when b = 0 -> set [set []]
    | (_, _) -> Set.union (allSubsets (n-1) k) (Set.map (fun x -> Set.union x (set [n])) (allSubsets (n-1) (k-1)))

let printAllSubsets n k = 
    Set.map (fun (x : int Set) -> System.Console.WriteLine x) (allSubsets n k) |> ignore
    ()