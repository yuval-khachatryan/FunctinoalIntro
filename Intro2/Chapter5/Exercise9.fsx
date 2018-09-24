(* Exercise 9
   Declare a function makeBill that gives a purchase map on the basis 
   of a list of items *)

type ArticleCode = string
type ArticleName = string
type NoPieces = int
type Price = int

type Register = Map<ArticleCode, ArticleName * Price>
type Info = NoPieces * ArticleName * Price
type InfoSeq = Info list
type Bill = InfoSeq * Price
type Purchase = ArticleCode * NoPieces list

let makeBill2 register purchase = 
    let calcPrice (purchaseMap, billPrice) articleCode numParts  = 
        try 
            let (articleName, articlePrice) = Map.find articleCode register
            let totalPrice = numParts * articlePrice  
            let articleStatus = Map.tryFind articleName purchaseMap
            match articleStatus with
            | Some (currentAmount, currentTotalPrice) -> 
                (Map.add articleName 
                         (currentAmount + numParts, currentTotalPrice + currentTotalPrice) 
                         (Map.remove articleName purchaseMap),
                 billPrice + totalPrice)
            | None -> (Map.add articleName (numParts, totalPrice) purchaseMap,
                       billPrice + totalPrice)
        with 
        | _ -> failwith "Article code not found" 
    List.fold (fun currentBill (ac, np) -> calcPrice currentBill ac np)
              (Map.empty, 0)
              purchase
    
let reg = Map.ofList [("a1", ("cheese", 25));
                      ("a2", ("herring", 4));
                      ("a3", ("soft drink", 5))]

let pur = [("a2", 3); ("a1", 1); ("a1", 3)]

makeBill2 reg pur
