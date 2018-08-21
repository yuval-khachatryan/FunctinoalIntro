namespace Exercise1

module Vector =
    type Vector = {x: float; y: float}
    type Vector with
        static member (~- ) {x = a; y = b} = {x = -a; y = -b}
        static member ( + ) ({x = x1; y = y1}, {x = x2; y = y2}) = {x = x1 + x2; y = y1 + y2}
        static member ( - ) ({x = x1; y = y1}, {x = x2; y = y2}) = {x = x1 - x2; y = y1 - y2}
        static member ( * ) (c, {x = x1; y = y1}) = {x = c * x1; y = c * y1}
        static member ( * ) ({x = x1; y = y1}, {x = x2; y = y2}) = x1 * x2 + y1 * y2
    let make(x1, y1) = {x = x1; y = y1}
    let coord{x = x1; y = y1} = (x1, y1)
    let norm (v : Vector) = System.Math.Sqrt (v * v)
