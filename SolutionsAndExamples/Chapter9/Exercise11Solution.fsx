open System.Collections.Generic

type BinTree<'T> = | Leaf | Node of BinTree<'T> * 'T * BinTree<'T>

let leftTree n =
    let rec iter m  acc = 
        match m with
        | 0 -> acc
        | _ -> iter (m-1) (Node(acc, n-m + 1, Leaf))
    iter n (Node(Leaf, 0, Leaf))

let rightTree n = 
    let rec iter m acc = 
        match m with
        | 0 -> acc
        | _ -> iter (m-1) (Node(Leaf, n-m+1, acc))
    iter n (Node(Leaf, 0, Leaf))
    
let rec count tree =
    match tree with
    | Leaf -> 0
    | Node(tl, x, tr) -> (count tl) + (count tr) + 1

let rec countC tree cont = 
    match tree with
    | Leaf -> cont 0
    | Node(tl, x, tr) -> countC tl (fun vl -> countC tr (fun vr -> cont(vl + vr + 1)))

let rec countA state tree = 
    match tree with
    | Leaf -> state
    | Node(tl, x, tr) -> 
        let newState = countA (state+1) tl
        countA newState tr

let rec countAC tree state continuation = 
    match tree with
    | Leaf -> continuation state
    | Node(tl, x, tr) -> countAC tl (state+1) (fun res -> countAC tr res continuation)

let rec preOrder tree = 
    match tree with
    | Leaf -> []
    | Node(tl, x, tr) -> x::(preOrder tl)@(preOrder tr) 

let rec preOrderC tree continuation = 
    match tree with
    | Leaf -> continuation []
    | Node(tl, x, tr) -> preOrderC tl (fun listL -> preOrderC tr (fun listR -> continuation(x::listL@listR)) )

let rec reversePreOrderAC tree acc continuation =
    match tree with
    | Leaf -> continuation acc
    | Node(tl, x, tr) -> reversePreOrderAC tl (x::acc) (fun listR -> reversePreOrderAC tr listR continuation)

let rec completeBinary n continuation = 
    match n with
    | 0 -> continuation (Node(Leaf, 0, Leaf) )
    | _ -> completeBinary (n-1) (fun bt -> continuation(Node(bt, n, bt )) )

let rec tryFind1 p s = 
    if Set.isEmpty s then None
    else let minE = Set.minElement s
         if p minE then Some minE
         else tryFind1 p (Set.remove minE s)

let enumerator (m : IEnumerable<'T>) = 
    let e = m.GetEnumerator()
    let f() = 
        match e.MoveNext() with
        | false -> None
        | _     -> Some (e.Current)
    f

let tryFind2 p (s: Set<'T>) = 
    let f = enumerator s
    let rec tFnd () = 
        match f() with
        | None -> None
        | Some x -> 
            if (p x) then Some x else tFnd()
    tFnd()


/// tail recursive version
let rec iterListR f lst = 
    match lst with
    | [] -> ()
    | x::xs -> f xs; iterListR f xs

// enumerator based version
let iterListE f (lst: 'T list) =
    let enm = enumerator lst
    let rec iter() = 
        match enm() with
        | None -> ()
        | Some x -> f x; iter()
    iter()

let rec iterSetR f s = 
    if Set.isEmpty s then ()
    else let minE = Set.minElement s
         f minE
         iterSetR f (Set.remove minE s)

let iterSetE f (s : Set<'T>) =
    let enm = enumerator s
    let rec iter() = 
        match enm() with
        | None -> ()
        | Some x -> f x; iter()
    iter()

#time
let mutable lst = []
for i in 1000000 .. -1 .. 1 do
    lst <- (i::lst)
    
let mutable s = Set.empty
for i in 1 .. 10000000 do
    s <- (Set.add i s)

for i in 1 .. 100 do iterListR ignore lst
for i in 1 .. 100 do iterListE ignore lst
iterSetR ignore s
iterSetE ignore s