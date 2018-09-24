namespace SolutionsLib

module Polynomial =
    [<Sealed>]
    type Polynomial =
        member Item : int -> int with get
        static member ( + ) : Polynomial * Polynomial -> Polynomial
        static member ( - ) : Polynomial * Polynomial -> Polynomial
        static member ( * ) : int * Polynomial -> Polynomial
        static member ( * ) : Polynomial * Polynomial -> Polynomial 
    val make : int list -> Polynomial