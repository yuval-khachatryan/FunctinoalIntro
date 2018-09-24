type Circuit<'a> = | Comp of 'a
                   | Ser of Circuit<'a> * Circuit<'a>
                   | Par of Circuit<'a> * Circuit<'a>

let cmp = Ser(Par(Comp 1.0, Comp 0.24), Comp 1.5)

let rec count = function
    | Comp _      -> 1
    | Ser(c1, c2) -> count c1 + count c2
    | Par(c1, c2) -> count c1 + count c2

let rec resistance = function
    | Comp r      -> r
    | Ser(c1, c2) -> resistance c1 + resistance c2
    | Par(c1, c2) -> 1 / (1 / (resistance c1) + 1 / (resistance c2) )

let rec circRec (c, s, p) = function
    | Comp  r    -> c r
    | Ser (x, y) -> s (circRec (c, s, p) x) (circRec (c, s, p) y)
    | Par (x, y) -> p (circRec (c, s, p) x) (circRec (c, s, p) y)

let count1 circ = circRec ( (fun _ -> 1), (+), (+) ) circ