namespace MyLibrary

module Vector =
    [<Sealed>]
    type Vector =
        static member ( ~- ) : Vector -> Vector
        static member ( +  ) : Vector * Vector -> Vector
        static member ( -  ) : Vector * Vector -> Vector
        static member ( *  ) : float * Vector -> Vector
        static member ( *  ) : Vector * Vector -> float
    val norm    : Vector -> float
    val make    : float * float -> Vector
    val coord   : Vector -> float * float

