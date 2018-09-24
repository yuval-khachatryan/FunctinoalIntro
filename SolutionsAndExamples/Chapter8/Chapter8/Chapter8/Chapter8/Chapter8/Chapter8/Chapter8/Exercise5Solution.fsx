let gcd n m = 
    let mutable a = if m < 0 then -m else m
    let mutable b = if n < 0 then -n else n

    while b > 0 do  
        let r = a % b
        a <- b
        b <- r
    a

