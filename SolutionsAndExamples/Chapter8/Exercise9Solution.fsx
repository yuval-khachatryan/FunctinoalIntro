open System.Collections.Generic


type ListTree<'T> = Node of 'T * (ListTree<'T> list)

let breadthFirstIter f ltr = 
    let remains = Queue<ListTree<'T>>()
    remains.Enqueue ltr
    while remains.Count <> 0 do
        let (Node (x,tl)) = remains.Dequeue()
        List.iter (remains.Enqueue) tl
        f x

/// returns the list of nodes in the tree obtained by traversing it
/// in breadth first order 
let breadthFirst ltr = 
    let mutable reverseResult = []
    breadthFirstIter (fun x -> (reverseResult <- x::reverseResult)) ltr
    List.rev reverseResult

let breadthFirstFold f acc ltr =
    let mutable result = acc
    breadthFirstIter (fun x -> result <- (f result x)) ltr
    result

let breadthFirst2 f ltr = 
    let remains = Queue<'T>()
    remains.Enqueue(ltr)
    let rec iter() = 
        if remains.Count > 0 
        then let (Node(x, tl)) = remains.Dequeue()
             List.iter (fun x -> remains.Enqueue x) tl
             f x
             iter()
        else ()
    iter()

let node1 = Node(1, [])
let node2 = Node(2, [])
let node3 = Node(3, [])
let node4 = Node(4, [])
let node5 = Node(5, [])

let node123 = Node(123, [node1; node2; node3])
let node45 = Node(45, [node4; node5]);
let node12345 = Node(12345, [node123; node45])

breadthFirst node12345

breadthFirstFold (fun acc x -> acc + (string x)) "" node12345