let length lst = 
    let rec lengthI (xs, res) =
        match xs with
        | [] -> res
        | h::t -> lengthI (t, res+1)
    lengthI (lst, 0)