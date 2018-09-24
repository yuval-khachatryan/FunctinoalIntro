type BinTree<'T> = | Leaf | Node of BinTree<'T> * 'T * BinTree<'T>

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

let t1 = Node(Leaf, 1, Leaf)
let t2 = Node(Leaf, 2, Leaf)
let t3 = Node(t1, 1, t2)
let t4 = Node(Leaf, 4, Leaf)
let t5 = Node(Leaf, 6, Leaf)
let t6 = Node(t4, 4, t5)
let t7 = Node(t3, 3, t6)

countA 0 t7
countAC t7 0 id