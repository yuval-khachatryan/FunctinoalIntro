type ListTree<'a> = Node of 'a * (ListTree<'a> list)

let rec depthFirst (Node(x, ts)) =
    x :: (List.collect depthFirst ts)

let rec depthFirstFold f e (Node(x, ts)) = 
    List.fold (depthFirstFold f) (f e x) ts

let rec breadthFirstList = function
    | [] -> []
    | (Node(x, ts)) :: rest -> 
        x :: breadthFirstList (rest @ ts)
    
let breadthFirst t = breadthFirstList [t]


let rec breadthFirstFoldBackList f tls e = 
    match tls with
    | [] -> e
    | (Node(x, ts)) :: rest -> f x (breadthFirstFoldBackList f (rest@ts) e)

let breadthFirstFoldBack f t e = breadthFirstFoldBackList f [t] e

type FileSys = Element list
and Element = | File of string
              | Dir of string * FileSys

let rec namesFileSys prefix = function
    | []    -> []
    | e::es -> (namesElement prefix e) @ (namesFileSys prefix es)
and namesElement prefix = function
    | File s -> [prefix + s]
    | Dir(s, fs) -> (prefix+s)::(namesFileSys (prefix+s+"/")  fs)

let d1 = 
    Dir("d1", [File "a1";
               Dir ("d2", [File "a2"; Dir("d3", [File "a3"])]);
               File "a4";
               Dir ("d3", [File "a5"])])


