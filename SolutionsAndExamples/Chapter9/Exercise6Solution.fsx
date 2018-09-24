(* A continuation version of the factorial function *)

let rec factC n cont = 
    if n <= 0 then cont 1 else factC (n-1) (fun x -> cont (n * x) )

let rec factA (n, m) = 
    if n <= 0 then m else factA(n-1, m*n)

#time

for i in 1 .. 10000000 do factA(10, 0) |> ignore
for i in 1 .. 10000000 do factC 10 id |> ignore

