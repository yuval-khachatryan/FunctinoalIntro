namespace Exercise2

module Complex =
    type Complex

    type Complex with
        static member (~- ) : Complex -> Complex
        static member ( + ) : Complex * Complex -> Complex
        static member ( - ) : Complex * Complex -> Complex
        static member ( * ) : Complex * Complex -> Complex
        static member ( / ) : Complex * Complex -> Complex
    val conjugate : Complex -> Complex
    val modulus : Complex -> float
    val argument : Complex -> float

