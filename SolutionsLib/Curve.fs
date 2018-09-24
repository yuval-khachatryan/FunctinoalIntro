namespace SolutionsLib


module Curve = 
    type Curve = C of (float * float) * ((float * float) list)
    
    let map f (C(p0, ps)) = C(f p0, List.map f ps)
    let mapP f (C(p0, ps)) = C(p0, List.map (f p0) ps)

    type Curve with 
        static member ( + ) (C(p0, ps), C(q0, qs)) =
            C(p0, ps@(q0::qs) )
        static member ( * ) (a, c: Curve) = 
            let multA (x0, y0) (x, y) = (x0 + a * (x-x0), y0 + a * (y-y0))
            mapP multA c
        static member ( |^) (c: Curve, angle) = 
            let radAngleRatio = System.Math.PI / 180.0
            let cs = cos(radAngleRatio * angle)
            let sn = sin(radAngleRatio * angle)
            let rotate (x0, y0) (x, y) = 
                let (dx, dy) = (x-x0, y-y0)
                (x0 + cs * dx - sn * dy, y0 + sn * dx + cs * dy)
            mapP rotate c
        static member ( |^) (c: Curve, angle: int) = c |^ (float angle)
        static member (-->) (c: Curve, (x1,y1) : float * float) = 
            match c with
            | C((x0, y0), _) -> map (fun (x,y) -> (x + x1 - x0, y + y1 - y0)) c
        static member ( ><) (c: Curve, a) = 
            map (fun (x,y) -> 2.0 * a - x, y) c
    let point p = C(p, [])
    let verticalReflection (c: Curve) b = 
        map (fun (x, y) -> x, 2.0 * b - y) c
    let boundingBox (C((a,b), ps)) =
        List.fold
            (fun ((minX, maxX), (minY, maxY)) (x, y) -> 
                ((min minX x, max maxX x), (min minY y, max maxY y) ) )
            ((a, a), (b, b))
            ps
    let width c = 
        let ((minX, maxX), _) = boundingBox c
        maxX - minX
    let height c = 
        let (_, (minY, maxY)) = boundingBox c
        maxY - minY
    
    let start (C (p0, ps)) = p0
    
    let toList (C(p, ps)) = p::ps