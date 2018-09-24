let rec fibA num current prev =
    if num <= 0 then current + prev else fibA (num - 1) (current + prev) current

let rec fibC num cont = 
    match num with
    | 0 -> cont 0
    | 1 -> cont 1
    | n -> fibC (n-1) (fun res1 -> fibC (n-2) (fun res2 -> cont(res1 + res2) ) ) 

#time
for i in 1 .. 40 do fibA i 0 1 |> ignore

for j in 1 .. 40 do fibC j id |> ignore

fibA 40 0 1 