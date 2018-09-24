(* Reimplement declarations makeBill3 using Map.fold instead of Map.foldBack *)
type ArticleCode = string
type ArticleName = string
type NoPieces = int
type Price = int

type Register = Map<ArticleCode, ArticleName * Price>
type Info = NoPieces * ArticleName * Price
type InfoSeq = Info list
type Bill = InfoSeq * Price
type Purchase = Map<ArticleCode, NoPieces>

let makeBill register purchase = 
    let calcPrice (infos, billPrice) articeCode numParts  = 
        try 
            let (articleName, articlePrice) = Map.find articeCode register
            let totalPrice = numParts * articlePrice  
            ((numParts, articleName, totalPrice)::infos, totalPrice + billPrice)
        with 
        | _ -> failwith "Article code not found" 
    Map.fold calcPrice ([], 0) purchase
    
let reg = Map.ofList [("a1", ("cheese", 25));
                      ("a2", ("herring", 4));
                      ("a3", ("soft drink", 5))]

let pur = Map.ofList [("a2", 3); ("a1", 1)]

makeBill reg pur