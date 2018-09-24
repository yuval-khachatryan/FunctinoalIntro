// Implementation of List.filter using List.foldBack

let listFilter predicate lst = 
    List.foldBack (fun x l -> ( if predicate x then x::l else l) ) lst []
 
