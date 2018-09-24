open System.Runtime.InteropServices
open System.Runtime.InteropServices

type AvlTree<'a when 'a : comparison> = 
    | Empty
    | Node of 'a * int * AvlTree<'a> * AvlTree<'a>
 
let leftSubTree t =
    match t with
    | Empty -> Empty
    | Node(v, h, tl, tr) -> tl

let rightSubTree t =
    match t with
    | Empty -> Empty
    | Node(v, h, tl, tr) -> tr

let rec height = function
    | Empty -> -1
    | Node (v, h, tl, tr) -> h

let makeTree value left right =
    Node(value, (max (height left) (height right) ) + 1, left, right)

let updateHeight t =
    match t with
    | Empty -> Empty
    | Node(v, h, tl, tr) -> Node(v, ( (max (height tl) (height tr)) + 1), tl, tr)


let rec insert x t =
    match t with
    | Empty                           -> Node (x, 0, Empty, Empty)
    | Node (v, h, tl, tr) when x <= v -> 
        let newTl = insert x tl
        makeTree v newTl tr
    | Node (v, h, tl, tr) ->
        let newTr = insert x tr
        makeTree v tl newTr

let rec minNode t =
    match t with
    | Empty -> None
    | Node(v, _, Empty, _) -> Some v
    | Node(v, _, tl, _) -> minNode tl

let rec maxNode t =
    match t with
    | Empty -> None
    | Node(v, _, _, Empty) -> Some v
    | Node(v, _, _, tr)    -> maxNode tr

let rec removeRight t = 
    match t with
    | Empty -> (Empty, None)
    | Node(v, _, tl, Empty) -> (tl, Some v)
    | Node(v, _, _, tr)     -> removeRight tr

let rec removeLeft t =
    match t with
    | Empty -> (Empty, None)
    | Node(v, _, Empty, tr) -> (tr, Some v)
    | Node(v, _, tl, _)     -> removeLeft tl

let rec remove x t = 
    match t with
    | Empty -> Empty
    | Node(v, h, Empty, tr) when x = v -> tr
    | Node(v, h, tl, Empty) when x = v -> tl
    | Node(v, h, tl, tr) when x = v ->
        match removeRight tl with
        | (_, None) -> tr
        | (st, Some a) -> makeTree a st tr
    | Node(v, h, tl, tr) when x < v ->
        let newTl = remove x tl
        makeTree x newTl tr
    | Node(v, h, tl, tr) ->
        let newTr = remove x tr
        makeTree x tl newTr

let singleRotateRight t =
    match t with
    | Empty -> Empty
    | Node(v, h, Node(vl, hl, tll, tlr), tr) ->
        let newRight = makeTree v tlr tr
        makeTree vl tll newRight
    | _ -> t

let singleRotateLeft t = 
    match t with
    | Empty -> Empty
    | Node(v, _, tl, Node(vr, hr, trl, trr)) ->
        let newLeft = makeTree v tl trl
        makeTree vr newLeft trr
    | _ -> t

let doubleRotateLeftRight t =
    match t with
    | Empty -> Empty
    | Node(v, h, tl, tr) ->
        (makeTree v (singleRotateLeft tl) tr) |> singleRotateRight

let doubleRotateRightLeft t =
    match t with
    | Empty -> Empty
    | Node(v, h, tl, tr) -> 
        (makeTree v tl (singleRotateRight tr) ) |> singleRotateLeft

let balanceFactor t = 
    match t with
    | Empty -> 0
    | Node(v, _, tl, tr) -> (height tl) - (height tr)

let balance t =
    if (balanceFactor t) <= 1 && -1 <= (balanceFactor t) 
    then t
    else if balanceFactor t >= 2
         then if balanceFactor (leftSubTree t) >= 0 then (singleRotateRight t) else (doubleRotateLeftRight t)
         else if balanceFactor (rightSubTree t) <= 0 then (singleRotateLeft t) else (doubleRotateRightLeft t)

let rec avlInsert x t =
    match t with
    | Empty -> (makeTree x Empty Empty)
    | Node (a, _, tl, tr) when x <= a -> balance (makeTree a (avlInsert x tl) tr)
    | Node (a, _, tl, tr) -> balance (makeTree a tl (avlInsert x tr) )

let rec avlRemove x t = 
    let rec avlRemoveLeft = function
    | Empty -> (Empty, None)
    | Node(a, _, Empty, tr) -> (balance tr, Some a)
    | Node(a, _, tl, tr) -> 
        match avlRemoveLeft tl with
        | (_,  None) -> failwith "Invalid value"
        | (st, Some v) -> ( balance (makeTree a st tr), Some v)
    let rec avlRemoveRight  = function
    | Empty -> (Empty, None)
    | Node(a, _, tl, Empty) -> (balance tl, Some a)
    | Node(a, _, tl, tr) -> 
        match avlRemoveRight tr with
        | (_, None) -> failwith "Invalid value"
        | (st, Some v) -> ( balance (makeTree a tl st), Some v)
    match t with
    | Empty -> Empty
    | Node(a, _, Empty, Empty) when a <> x -> t
    | Node(a, _, Empty, Empty) when a = x -> Empty
    | Node(a, _, tl, tr) when x < a -> balance (makeTree a (avlRemove x tl) tr )
    | Node(a, _, tl, tr) when x > a -> balance (makeTree a tl (avlRemove x tr) )
    | Node(a, _, tl, tr) -> 
        match avlRemoveRight tl with
        | (st, None) -> tr
        | (st, Some v) -> balance (makeTree v st tr)

let rec printTree (t : AvlTree<string>)  = 
    let rec printTabs n = 
        if n <= 0 
        then () 
        else System.Console.Write "\t"
             printTabs (n-1)
    let rec printNode (node : AvlTree<string>) (height : int) = 
        match node with
        | Empty -> ()
        | Node(a, _, Empty, Empty) -> 
            printTabs height
            System.Console.WriteLine a 
        | Node(a, _, tl, tr) ->
            printNode tr (height + 1)
            printTabs height
            System.Console.WriteLine a
            printNode tl (height + 1)
    match t with
    | Empty -> ()
    | Node (v, h, tl, tr) -> printNode t 0 

let t0 = avlInsert "a" Empty
let t1 = avlInsert "b" t0
let t2 = avlInsert "1a" t1
let t3 = avlInsert " " t2
let t4 = avlInsert "a3" t3
let t5 = avlInsert "a4" t4
let t6 = avlInsert "b6" t5

let s0 = avlInsert "0" Empty
let s1 = avlInsert "1" s0
let s2 = avlInsert "2" s1
let s3 = avlInsert "3" s2
let s4 = avlInsert "4" s3
let s5 = avlInsert "5" s4
let s6 = avlInsert "6" s5
let s7 = avlInsert "7" s6
let s8 = avlInsert "6a" s7
let s9 = avlInsert "5a" s8
let s10 = avlInsert "6ab" s9
let s11 = avlInsert "5ab" s10

let u10 = avlRemove "0" s10
let u91 = avlRemove "3" u10
let u92 = avlRemove "2" u10