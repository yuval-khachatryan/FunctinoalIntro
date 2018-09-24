#r "D:/Documents/LearnF#/Functional Programming using F#/FunctinoalIntro/SolutionsLib/bin/Debug/netstandard2.0/SolutionsLib.dll"
#r "D:/Documents/LearnF#/Functional Programming using F#/FunctinoalIntro/GraphicsLib/obj/Debug/GraphicsLib.dll"

open SolutionsLib
open GraphicsLib

SimpleDrawing.display ("Sierpinski Curve", SimpleDrawing.adjust(Fractals.sierpinski 8, 3.0) )
SimpleDrawing.display ("Hilbert Curve", SimpleDrawing.adjust(Fractals.peano 6, 2.0) )

