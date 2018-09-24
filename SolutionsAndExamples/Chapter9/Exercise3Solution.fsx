let rec sum(m, n) = 
    if n = 0 then m
    else m + sum(m+1, n-1)

let rec sumA(m, n, a) = 
    if n >= 0 then sumA(m+1, n-1, a + m) else a