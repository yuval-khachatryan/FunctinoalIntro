namespace MyLibrary


module Queue = 
    [<Sealed>]
    type Queue<'a when 'a : comparison> =
        interface System.IComparable
        member Item : int -> 'a with get
    val empty : Queue<'a>
    val put   : 'a -> Queue<'a> -> Queue<'a>
    val get   : Queue<'a> -> 'a * Queue<'a>
    exception EmptyQueue
