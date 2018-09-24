open System.Collections.Generic

let enumerator (m: IEnumerable<'c>) = 
    let e = m.GetEnumerator()
    let f() =
        match e.MoveNext() with
        | false -> None
        | _ -> Some(e.Current)
    f

let HashSetFold f acc (hs : HashSet<'T>) = 
    let mutable result = acc
    let getNext = enumerator hs
    let rec iter() = 
        match getNext() with
        | None -> result
        | Some x -> 
            result <- f result x
            iter()
    iter()
    
let hs = HashSet<int>()
hs.Add(5)
hs.Add(3)
hs.Add(2)

let mutable rslt = 0
let getNext() = enumerator hs

HashSetFold (fun a b -> a@[b] ) [] hs