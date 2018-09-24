open System.Collections.Generic

let enumerator (c: IEnumerable<'T>) = 
    let e = c.GetEnumerator()
    let f() =
        match e.MoveNext() with
        | false -> None
        | _     -> Some (e.Current)
    f

let DictionaryFold f acc (d: IDictionary<'T, 'S>) =
    let mutable res = acc
    let getNext = enumerator d
    let rec iter() = 
        match getNext() with
        | None -> res
        | Some entry -> res <- (f res entry.Key entry.Value)
                        iter()
    iter()

let dict = SortedDictionary<int, string>()
dict.Add(1, "one")
dict.Add(2, "two")
dict.Add(3, "three")

let members = List.rev (DictionaryFold (fun x a b -> (a,b)::x) [] dict )

let hashDict = Dictionary<int, string>()
hashDict.Add(4, "four")
hashDict.Add(2, "2")
hashDict.Add(3, "3")
hashDict.Add(1, "1")

let hMembers = List.rev (DictionaryFold (fun x a b -> (a,b)::x) [] hashDict)