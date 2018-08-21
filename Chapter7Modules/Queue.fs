namespace MyLibrary

module Queue =
    exception EmptyQueue
    [<CustomEquality; CustomComparison>]
    type Queue<'a when 'a : comparison> = 
        {front: 'a list; rear: 'a list}
        member q.list() = q.front @ (List.rev q.rear)
        interface System.IComparable with
            member q1.CompareTo qobj =
                match qobj with
                | :? Queue<'a> as q2 -> compare (q1.list()) (q2.list())
                | _ -> invalidArg (qobj.ToString()) "cannot compare values of different types"
        member q.Item 
            with get n = (q.list()).[n]
        override q1.Equals qobj = 
            match qobj with
            | :? Queue<'a> as q2 -> q1.list() = q2.list()
            | _ -> false
        override q.GetHashCode() = hash (q.list())
        override q.ToString() = string (q.list())
    let empty = {front = []; rear = []}
    let put y {front = xs; rear = ys} = {front = xs; rear = y::ys}
    let rec get = 
        function
        | {front = x::xs; rear = ys} -> (x, {front = xs; rear = ys})
        | {front = []; rear = [] } -> raise EmptyQueue
        | {front = []; rear = ys } -> get {front = List.rev ys; rear = []}

