let fibonacci n = 
    if n <= 1 then 1 
    else 
        let mutable count = 2
        let mutable prev = 1
        let mutable current = 2
        while count <= n do
            let temp = prev + current
            prev <- current
            current <- temp
            count <- count + 1
        current 

let mutable count = 0
while count <= 10 do
    System.Console.WriteLine(fibonacci count)
    count <- count + 1