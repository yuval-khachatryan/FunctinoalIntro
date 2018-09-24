type Node<'T> = {mutable link: Node<'T> option; data: 'T}

let insertBefore node value = 
    {link = Some node; data = value}

let insertAfter node value =
    node.link <- Some {link = node.link; data = value}
    node

let rec insertOrdered node value = 
    match node with
    | {link = next; data = x} when value <= x -> insertBefore node value
    | {link = None; data = x}  -> insertAfter node value
    | {link = Some next; data = x} when value <= next.data -> insertAfter node value
    | {link = Some next; data = x} -> insertOrdered next value

let n1 = {link = None; data = 5}
let n2 = insertOrdered n1 6
let n3 = insertOrdered n2 4
let n4 = insertOrdered n3 10
let n5 = insertOrdered n3 2


