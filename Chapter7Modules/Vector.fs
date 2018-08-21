namespace MyLibrary

module Vector = 
    type Vector =  V of float * float
    
    let norm (V(x, y)) = System.Math.Sqrt (x * x + y * y)
    let make (x, y) = V(x, y)
    let coord (V(x, y)) = (x, y)
    
    type Vector with
        static member (~- ) (V(x, y)) = V(-x, -y)
        static member ( + ) (V(x1, y1), V(x2, y2)) = V(x1 + x2, y1 + y2)
        static member ( - ) (V(x1, y1), V(x2, y2)) = V(x1 - x2, y1 - y2)
        static member ( * ) (c, V(x, y)) = V(c * x, c * x)
        static member ( * ) ( V(x1, y1), V(x2, y2) )= x1 * x2 + y1 * y2
    

