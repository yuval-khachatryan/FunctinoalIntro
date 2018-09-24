#r "D:/Documents/LearnF#/Functional Programming using F#/FunctinoalIntro/SolutionsLib/bin/Debug/netstandard2.0/SolutionsLib.dll"
#r "D:/Documents/LearnF#/Functional Programming using F#/FunctinoalIntro/GraphicsLib/obj/Debug/GraphicsLib.dll"
open SolutionsLib
open GraphicsLib

let h0 = Curve.point(0.0, 0.0)

let hilbert hn = 
    let w = Curve.width hn
    let h = Curve.height hn
    let c0 = hn >< 0.0
    let c1 = c0 |^ -90
    let c2 = hn --> (0.0, w + 1.0)
    let c3 = hn --> (h + 1.0, w + 1.0)
    let c4 = (c0  |^ 90) --> (h + h + 1.0, w)
    c1 + c2 + c3 + c4



let adjust(c: Curve.Curve, a:float) =
    let c1 = a * c --> (10.0, 10.0)
    let (_, (maxX, maxY)) = Curve.boundingBox c1
    let pw = int (round maxX) + 20
    let ph = int (round maxY) + 20
    (c1, pw, ph)

let rec hilbertCurve n = 
    if n <= 0 then h0 else hilbert (hilbertCurve (n-1) )

SimpleDrawing.display("Hilbert Curve 1", adjust(hilbertCurve 12, 5.0))


