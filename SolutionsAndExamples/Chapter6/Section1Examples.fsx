open System.Numerics
open System.Web.Management

/// Section 1 - Chinese boxes
type Colour = Red | Blue | Green | Yellow | Purple
type Cbox = | Nothing
            | Cube of float * Colour * Cbox

let count = 
    let rec iterCount acc = function 
    | Nothing -> acc
    | Cube(r, c, cb) -> iterCount (acc + 1) cb
    iterCount 0

let rec insert r c cb =
    if r <= 0.0 then failwith "Negative radius of ChineseBox"
    else match cb with
         | Nothing -> Cube(r, c, Nothing)
         | Cube(r1, c1, cb1) when r1 < r -> Cube(r, c, cb)
         | Cube(r1, c1, cb1) when r1 = r -> failwith "Inserting chinese box into another of same size"
         | Cube(r1, c1, cb1) -> Cube(r1, c1, insert r c cb1)

let cb1 = Cube(0.5, Red, Nothing)
let cb2 = Cube(1.0, Green, cb1)
let cb3 = Cube(2.0, Yellow, cb2)

insert 2.0 Yellow (insert 1.0 Green Nothing)

type Fexpr = | Const of float
             | X
             | Add of Fexpr * Fexpr
             | Sub of Fexpr * Fexpr
             | Mul of Fexpr * Fexpr
             | Div of Fexpr * Fexpr
             | Sin of Fexpr
             | Cos of Fexpr
             | Log of Fexpr
             | Exp of Fexpr
 
let addExpr fe ge = Add(fe, ge)
let subExpr fe ge = Sub(fe, ge)
let mulExpr fe ge = Mul(fe, ge)
let divExpr fe ge = Div(fe, ge)
let sinExpr fe = Sin fe
let cosExpr fe = Cos fe
let logExpr fe = Log fe
let expExpr fe = Exp fe

let comparePairs (x1, y1) (x2, y2) =
    if x1 < x2 
    then -1
    else if x1 > x2 
         then 1
         else if y1 < y2 
              then -1
              else if y1 = y2 then 0 else 1

let compareSingles x y = if x < y then -1 else (if x = y then 0 else 1)

let compare expression1 expression2 =
    match (expression1, expression2) with
    | (Const x, Const y)          -> if x < y then -1 else (if x = y then 0 else 1)
    | (Const _, _)                -> -1
    | (_, Const b)                -> 1
    | (X, X)                      -> 0
    | (X, _)                      -> -1
    | (_, X)                      -> 1
    | (Cos x, Cos y)              -> if x < y then -1 else (if x = y then 0 else 1)
    | (Cos _, _)                  -> -1
    | (_, Cos _)                  -> 1
    | (Sin x, Sin y)              -> if x < y then -1 else (if x = y then 0 else 1)
    | (Sin _, _)                  -> -1
    | (_, Sin _)                  -> 1
    | (Log x, Log y)              -> if x < y then -1 else (if x = y then 0 else 1)
    | (Log _, _)                  -> -1
    | (_, Log _)                  -> 1
    | (Exp x, Exp y)              -> if x < y then -1 else (if x = y then 0 else 1)
    | (Exp _, _)                  -> -1
    | (_, Exp _)                  -> 1
    | (Add(x1, y1), Add(x2, y2))  -> comparePairs (x1, y1) (x2, y2)
    | (Add(_, _), _)              -> -1
    | (_, Add(_, _))              -> 1
    | (Sub (x1, y1), Sub(x2, y2)) -> comparePairs (x1, y1) (x2, y2)
    | (Sub (_, _), _)             -> -1
    | (_, Sub (_, _))             -> 1
    | (Mul(x1, y1), Mul(y2, x2))  -> comparePairs (x1, y1) (x2, y2)
    | (Mul(_, _), _)              -> -1
    | (_, Mul(_, _))              -> 1
    | (Div (x1, y1), Div(x2, y2)) -> comparePairs (x1, y1) (x2, y2)

let rec orderExpression =
    function
    | Const x   -> Const x
    | X         -> X
    | Cos x     -> Cos (orderExpression x)
    | Sin x     -> Sin (orderExpression x)
    | Log x     -> Log (orderExpression x)
    | Exp x     -> Exp (orderExpression x)
    | Add(x, Add(y, z)) when ( (compare x y) > 0) 
                -> orderExpression (Add(y, Add(x, z)))
    | Add(x, y) -> let (ordX, ordY) = (orderExpression x, orderExpression y)
                   match compare ordX ordY with
                   | 1 -> Add(ordY, ordX)
                   | _ -> Add(ordX, ordY)
    | Mul(x, Mul(y,z)) when ( (compare x y) > 0) 
                -> orderExpression (Mul(y, Mul(x, z)))
    | Mul(x, y) -> let (ordX, ordY) = (orderExpression x, orderExpression y)
                   match compare ordX ordY with
                   | 1 -> Mul(ordY, ordX)
                   | _ -> Mul(ordX, ordY)
    | Sub(x, y) -> Sub (orderExpression x, orderExpression y)
    | Div(x, y) -> Div (orderExpression x, orderExpression y)

let rec simplifyExpression fe =
    let rec simplifyUnary genExpr fe = 
        let simFe = simplifyExpression fe
        if fe = simFe then genExpr fe else simplifyExpression (genExpr simFe)
    let rec simplifyBinary genExpr fe ge =
        let simFe, simGe = simplifyExpression fe, simplifyExpression ge
        if (simFe, simGe) = (fe, ge) then genExpr fe ge else simplifyExpression (genExpr simFe simGe)
    let ordFe = orderExpression fe
    match ordFe with
    | Const x                           -> Const x
    | X                                 -> X
    | Cos (Const x)                     -> Const (System.Math.Cos x) 
    | Cos x                             -> Cos (simplifyExpression x) 
    | Sin (Const x)                     -> Const (System.Math.Sin x)
    | Sin x                             -> Sin (simplifyExpression x)
    | Log (Const x)                     -> Const (System.Math.Log x)
    | Log x                             -> Log (simplifyExpression x)
    | Exp (Const x)                     -> Const (System.Math.Exp x)
    | Exp x                             -> Exp (simplifyExpression x)
    | Add(Const a, Const b)             -> Const (a + b)
    | Add(x, Const 0.0)                
    | Add(Const 0.0, x) 
    | Sub(x, Const 0.0)
    | Mul(Const 1.0, x)
    | Mul(x, Const 1.0)                 
    | Div(x, Const 1.0)                 -> x
    | Mul(Const 0.0, x)                 
    | Mul(x, Const 0.0)
    | Div(Const 0.0, x)                 -> Const 0.0
    | Sub(x, y) when x = y              -> Const 0.0
    | Div(x, y) when x = y              -> Const 1.0
    | Add(x, y) when x = y              -> simplifyExpression (Mul(Const 2.0, x))
    | Add(x, Mul(y, z))
    | Add(x, Mul(z, y))
    | Add(Mul(y, z), x)
    | Add(Mul(z, y), x) when x = y      -> simplifyExpression (Mul(Add(Const 1.0, z), x))
    | Add(Mul(x1, y1), Mul(x2, y2)) 
    | Add(Mul(x1, y1), Mul(y2, x2)) 
    | Add(Mul(y1, x1), Mul(x2, y2)) 
    | Add(Mul(y1, x1), Mul(y2, x2)) when x1 = x2 -> simplifyExpression (Mul(x1, Add(y1, y2)))
    | Add(Div(x1, y1), Div(x2, y2))    -> simplifyExpression (Div (Add(Mul(x1, y2), Mul(x2, y1)),
                                                                  Mul(y1, y2)))
    | Add(x, y)                         -> simplifyBinary addExpr x y
    | Sub(Const a, Const b)             -> Const (a - b)
    | Sub(Const 0.0, x)                 -> simplifyExpression (Mul(Const -1.0, x) )
    | Sub(Mul(x1, y1), Mul(x2, y2)) 
    | Sub(Mul(x1, y1), Mul(y2, x2)) 
    | Sub(Mul(y1, x1), Mul(x2, y2)) 
    | Sub(Mul(y1, x1), Mul(y2, x2)) when x1 = x2 -> simplifyExpression (Mul(x1, Sub(y1, y2)))
    | Sub(Div(x1, y1), Div(x2, y2))     -> simplifyExpression (Div (Mul(Sub(x1, y2), Sub(y2, x1)),
                                                                   Mul(y1, y2)))
    | Sub(x, y)                         -> simplifyBinary subExpr x y
    | Mul(Const a,  Const b)            -> Const (a * b)
    | Mul(x, Div(y, z))                 -> simplifyExpression (Div(Mul(x, y), z))
    | Mul(Exp x, Exp y)                 -> simplifyExpression (Exp(Add(x, y)))
    | Mul(x, y)                         -> simplifyBinary mulExpr x y
    | Div(Const x, Const y)             -> Const (x / y)
    | Div(x, Div(y, z))                 -> simplifyExpression (Div(Mul(x, z), y))
    | Div(Div(x, y), z)                 -> simplifyExpression (Div(x, Mul(y, z)))
    | Div(x, y)                         -> simplifyBinary divExpr x y
    

let rec D = 
    function
    | Const _       -> Const 0.0
    | X             -> Const 1.0
    | Add(fe, ge)   -> Add(D fe, D ge)
    | Sub(fe, ge)   -> Sub(D fe, D ge)
    | Mul(fe, ge)   -> Add(Mul(fe, D ge), Mul(D fe, ge) )
    | Div(fe, ge)   -> Div(Sub(Mul(D fe, ge), Mul(fe, D ge) ),
                           Mul(ge, ge))
    | Sin fe        -> Mul(D fe, Cos fe)
    | Cos fe        -> Mul(Const -1.0, Mul(D fe, Sin fe))
    | Log fe        -> Sub(D fe, fe)
    | Exp fe        -> Mul(fe, Exp fe)


let x3 = Mul(X, Mul(X, X))

D x3 |> simplifyExpression 