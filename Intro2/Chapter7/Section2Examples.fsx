#r "D:/Documents/LearnF#/Functional Programming using F#/FunctinoalIntro/Chapter7Modules/bin/Debug/netstandard2.0/Chapter7Modules.dll"

open MyLibrary

let a = Vector.make (1.0, -2.0)
let b = Vector.make (3.0, 4.0)
let c = 2.0 * a - b