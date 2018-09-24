type BinTree<'a, 'b> =
     | Leaf of 'a
     | Node of BinTree<'a, 'b> * 'b * BinTree<'a, 'b>

let t0 = Node(Node(Leaf 1, "cd", Leaf 2), "ab", Leaf 3)

let t1 = Node(Leaf 1, "cd", Leaf 2)
let t2 = Leaf 3

let rec depth = function
    | Leaf _            -> 0
    | Node(t1, _, t2)   -> 1 + max (depth t1) (depth t2)
    
type BinTree<'a when 'a : comparison> =
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>

let rec preOrder = function
    | Leaf -> []
    | Node(tl,x,tr) -> x :: (preOrder tl) @ (preOrder tr)

let rec inOrder = function
    | Leaf -> []
    | Node(tl, x, tr) -> (inOrder tl) @ [x] @ (inOrder tr)

let rec postOrder = function
    | Leaf -> []
    | Node(tl, x, tr) -> (preOrder tl) @ (preOrder tr) @ [x]

let preFold f e t = List.fold f e (preOrder t)
let preFoldBack f e t = List.foldBack f (preOrder t) e

let rec add x t = 
    match t with
    | Leaf                       -> Node(Leaf, x, Leaf)
    | Node(tl, a, tr) when x < a -> Node(add x tl, a, tr)
    | Node(tl, a, tr) when x > a -> Node(tl, a, add x tr)
    | _                          -> t

let rec contains x t = 
    match t with 
    | Leaf -> false
    | Node(tl, a, tr) when x < a -> contains x tl
    | Node(tl, a, tr) when x > a -> contains x tr
    | _                          -> true 

let t3 = Node(Node(Leaf, -3, Leaf), 0, Node(Leaf, 2, Leaf))
let t4 = Node(t3, 5, Node(Leaf, 7, Leaf))

inOrder t4
contains 0 t4
contains -1 t4