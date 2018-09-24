type DoublyLinkedNode<'T> = {data: 'T option; 
                             mutable prev: DoublyLinkedNode<'T> option;
                             mutable next: DoublyLinkedNode<'T> option}

let insertBefore node value = 
    let inserted = {data = Some value; prev = node.prev; next = Some node}
    node.prev <- Some inserted
    match inserted.prev with
    | None -> ()
    | Some p -> p.next <- Some inserted
    inserted

let insertAfter node value =
    let inserted = {data = Some value; prev = Some node; next = node.next}
    node.next <- Some inserted
    match inserted.next with
    | None -> ()
    | Some n -> n.prev <- Some inserted
    inserted

let valueOf node =
    match node with
    | {data = None; prev = _; next = _} -> failwith "The node doesn't contain a value"
    | {data = Some x; prev = _; next = _} -> x

let rec insertSorted node value =
    match node with
    | {data = None;   prev = p; next = n} -> {data = Some value; prev = None; next = None}
    | {data = Some x; prev = p; next = n} when x = value -> insertBefore node value
    | {data = Some x; prev = None; next = n} when value < x -> insertBefore node value
    | {data = Some x; prev = Some p; next = n} when value < x && valueOf p <= value -> insertBefore node value
    | {data = Some x; prev = Some p; next = n} when value < x -> insertSorted p value
    | {data = Some x; prev = p; next = None} when x < value -> insertAfter node value
    | {data = Some x; prev = p; next = Some n} when x < value && value <= valueOf n -> insertAfter node value
    | {data = Some x; prev = p; next = Some n} -> insertSorted n value
    | _ -> failwith "Unhandled case"

let remove node = 
    match node with
    | {data = _; prev = None; next = None} -> {data = None; prev = None; next = None}
    | {data = _; prev = Some p; next = None} -> p.next <- None; node.prev <- None; node.next <- None; p
    | {data = _; prev = None; next = Some n} -> n.prev <- None; node.prev <- None; node.next <- None; n
    | {data = _; prev = Some p; next = Some n} -> 
        n.prev <- Some p
        p.next <- Some n
        node.prev <- None
        node.next <- None
        p
        

let n1 = {data = Some 0; prev = None; next = None}
let n2 = insertSorted n1 -1
let n3 = insertSorted n2 -5
let n4 = insertSorted n3 -3
let n5 = insertSorted n4 20
let n6 = insertSorted n5 16
let n7 = insertSorted n1 3

remove n1
remove n2
remove n3
remove n4
remove n5
remove n6
remove n7