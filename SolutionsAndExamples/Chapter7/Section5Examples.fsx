type ObjVector(x : float, y : float) =
    member v.X = x
    member v.Y = y
    member v.Coord() = (v.X, v.Y)
    member v.Norm = System.Math.Sqrt (v.X * v.X + v.Y * v.Y)
    
    static member ( ~-) (v : ObjVector) = ObjVector(-v.X, -v.Y)
    static member ( + ) (v1 : ObjVector, v2: ObjVector) = ObjVector(v1.X + v2.X, v1.Y + v2.Y)
    static member ( - ) (v1 : ObjVector, v2: ObjVector) = ObjVector(v1.X - v2.X, v1.Y - v2.Y)
    static member ( * ) (a, v : ObjVector) = (a * v.X, a * v.Y)
    static member ( * ) (v1 : ObjVector, v2 : ObjVector) = v1.X * v2.X + v2.Y * v2.Y

let coord (v : ObjVector) = v.Coord()