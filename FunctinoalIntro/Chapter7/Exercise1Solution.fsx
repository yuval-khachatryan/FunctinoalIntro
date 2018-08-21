(*  
    This script only shows that examples from the section 1 work
    Refer to vector in SolutionsLib for the required implementation 
    of vector using records *)
#r "D:/Documents/LearnF#/Functional Programming using F#/FunctinoalIntro/SolutionsLib/bin/Debug/netstandard2.0/SolutionsLib.dll"
open Exercise1

let a = Vector.make(1.0, -2.0)

let b = Vector.make(3.0, -4.0)

let c = 2.0 * a - b

Vector.coord c

let d = c * a

let e = Vector.norm b

let g = (+) a b

Vector.coord g