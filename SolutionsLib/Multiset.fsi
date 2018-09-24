namespace SolutionsLib

module Multiset =
    [<Sealed>]
    type Multiset<'T when 'T : comparison and 'T : equality> =
        member Item : 'T -> int with get
        static member ( + ) : Multiset<'T> * Multiset<'T> -> Multiset<'T>
        static member ( - ) : Multiset<'T> * Multiset<'T> -> Multiset<'T>
        member elements : unit -> 'T list
        
    val count : 'T -> Multiset<'T>  -> int
    val insert : 'T -> Multiset<'T>  -> Multiset<'T>
    val intersect : Multiset<'T> -> Multiset<'T> -> Multiset<'T>
    val ofList : 'T list -> Multiset<'T>
