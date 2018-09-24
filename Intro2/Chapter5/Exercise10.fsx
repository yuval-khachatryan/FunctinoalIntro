(* Exercise 10
   Same as 9 but allows discounts to be applied *)

type ArticleCode = string
type ArticleName = string
type NoPieces = int
type Price = int
type DiscountRate = int

type ProductsInfo = Map<ArticleCode, ArticleName * Price>
type Discount = {amount: int; rate: int}
type Discounts = Map<ArticleCode, Discount>
type Register = ProductsInfo * Discounts
type Info = NoPieces * ArticleName * Price
type InfoSeq = Info list
type Bill = InfoSeq * Price
type Purchase = ArticleCode * NoPieces list

let makeBill2 (productsInfo, discounts) purchase = 
    let applyDiscount articleCode numParts =
        let discount =
            match Map.tryFind articleCode discounts with
            | None -> {amount = 1; rate = 0}
            | Some disc -> disc
        let price = 
            match Map.tryFind articleCode productsInfo with
            | None -> failwith "Invalid article code"
            | Some (an, p) -> p
        let discountFreeAmount = numParts % discount.amount
        let discountableAmount = numParts - discountFreeAmount
        discountableAmount * price * (100 - discount.rate) / 100 +  discountFreeAmount * price
    
    let updateItemsCount itemSet (ac, np) =
        match Map.tryFind ac itemSet with
        | None -> Map.add ac np itemSet
        | Some number -> Map.add ac (number + np) (Map.remove ac itemSet)
    
    let itemName ac =
        match Map.tryFind ac productsInfo with
        | Some (an, p) -> an
        | None -> failwith "Article code not found"
    
    let itemSet = List.fold updateItemsCount Map.empty purchase
    let purchaseSet = Map.map (fun ac np ->  (np, applyDiscount ac np)) itemSet
    let totalPrice = Map.fold (fun currentTotal _ (_, discPrice) -> discPrice + currentTotal) 0 purchaseSet
    let purchaseList = List.map (fun (k, v) -> (itemName k, v)) (Map.toList purchaseSet)
    (purchaseList, totalPrice)
    
let items = Map.ofList [("a1", ("cheese", 25));
                        ("a2", ("herring", 4));
                        ("a3", ("soft drink", 5))]
let discounts = Map.ofList[("a1", {amount = 3; rate = 33})
                           ("a3", {amount = 5; rate = 20})] 
let reg = (items, discounts)

let pur = [("a3", 7); ("a1", 1); ("a3", 8)]

makeBill2 reg pur

