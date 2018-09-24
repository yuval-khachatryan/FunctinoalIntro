#time

let rec factA(n, m) = if n <= 0 then m else factA(n-1, n * m)

let factW n = 
    let mutable ni = n
    let mutable result = 1
    while ni >= 0 do 
        result <- result * ni
        ni <- ni - 1
    result



let rec bigListC n c = 
    if n = 0 then c []
    else bigListC (n-1) (fun res -> c(1::res))


let rec bigListA (n, a) =
    if n = 0 then a
    else bigListA(n-1, 1::a)

type BinTree<'T> = | Leaf
                   | Node of BinTree<'T> * 'T * BinTree<'T>

let rec count  = function
    | Leaf -> 0
    | Node(tl, x, tr) -> (count tl) + (count tr) + 1

let rec countC t c = 
    match t with
    | Leaf -> c 0
    | Node(tl, x, tr) -> countC tl (fun vl -> countC tr (fun vr -> c (vl + vr + 1) ))

bigListA (1000000, [])
bigListC 1000000 (fun x -> x)