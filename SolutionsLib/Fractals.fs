namespace SolutionsLib

module Fractals = 
    let rec sierpinski = 
        function
        | n when n > 0 -> 
            let s = (sierpinski (n-1))
            let h = Curve.height s
            let w = Curve.width s
            let s1 = s
            let s2 = (s |^ 90) --> (w, h+1.0)
            let s3 = (s |^ -90) --> (1.0, w + h + 2.0)
            let s4 = s --> (2.0 + h, h + 2.0)
            s1 + s2 + s3 + s4
        | _ -> Curve.point (0.0, 0.0)
    
    let continueVertical  horizontalDir verticalDir (c: Curve.Curve) = 
        let w = Curve.width c
        let h = Curve.height c
        let (x0, y0) = Curve.start c
        (c >< x0) --> (x0 + horizontalDir * w, y0 + verticalDir * (h + 1.0))
    
    let continueHorizontal  horizontalDir verticalDir (c : Curve.Curve) =
        let w = Curve.width c  
        let h = Curve.height c
        let (x0, y0) = Curve.start c
        (Curve.verticalReflection c y0) --> (x0 + horizontalDir * (1.0 + w), y0 + verticalDir * h)

    let moveUpRight  (c: Curve.Curve) = continueVertical 1.0  1.0 c
    let moveUpLeft (c: Curve.Curve) = continueVertical -1.0 1.0 c
    let moveDownRight (c: Curve.Curve)  = continueVertical 1.0 -1.0  c
    let moveDownLeft   (c: Curve.Curve) = continueVertical      -1.0 -1.0 c
    let moveRightUp    (c: Curve.Curve) = continueHorizontal     1.0  1.0 c
    let moveRightDown  (c: Curve.Curve) = continueHorizontal     1.0 -1.0 c

    let rec peano = function
        | n when n > 0 -> 
            let h1 = peano (n-1)
            let h2 = moveUpRight h1
            let h3 = moveUpLeft h2
            let h4 = moveRightUp h3
            let h5 = moveDownRight h4
            let h6 = moveDownLeft h5
            let h7 = moveRightDown h6
            let h8 = moveUpRight h7
            let h9 = moveUpLeft h8
            h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 + h9
        | _  -> Curve.point (0.0, 0.0) 

    

