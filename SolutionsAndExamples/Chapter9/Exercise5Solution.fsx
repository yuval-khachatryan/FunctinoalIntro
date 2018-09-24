let fold folder state lst = 
    let rec itfold f (r, xs) = if xs <> [] then itfold f (f r (List.head xs), List.tail xs)
    itfold folder (state, lst)