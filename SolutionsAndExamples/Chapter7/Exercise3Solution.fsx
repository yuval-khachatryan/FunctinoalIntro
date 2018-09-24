(* Test of multiset implementation *)
#r "D:/Documents/LearnF#/Functional Programming using F#/FunctinoalIntro/SolutionsLib/bin/Debug/netstandard2.0/SolutionsLib.dll"

open Exercise3

let mul1 = Multiset.ofList [1;-5;2;-4;2;-3;4;6]
mul1.elements()

let mul2 = Multiset.ofList [1;-5;2;-5;2;-4;4;6]
mul2.elements()

let mul3 = mul1 + mul2
let mul4 = mul1 - mul2
let mul5 = mul2 - mul1

let a11 = mul1.[1]
let a12 = mul2.[1]
let a21 = mul1.[2]
let a22 = mul2.[2]
let a23 = mul3.[2]

let mul6 = Multiset.intersect mul1 mul2