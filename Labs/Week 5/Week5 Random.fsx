let a = set ["BOB";"ALICE"];;
let b = set ["ALICE";"BOB"];;
a = b

let newset = Set.ofList [1;2;3];;
Set.minElement a;;

Set.fold (+) -1 newset;;

let reg1 = Map.ofList [("a1",("cheese",25));("a2",("herring",4));("a3",("soft drink",5))];;
let reg2 = Map.add "a4" ("bread", 6) reg1;;

Map.find "a4" reg2;;
Map.tryFind "a2" reg1;;

type ArticleCode = string;;
type ArticleName = string;;
type NoPieces = int;;
type Price = int;;
type Info = NoPieces * ArticleName * Price;;
type Infoseq = Info list;;
type Bill = Infoseq * Price;;

type Register = Map<ArticleCode, ArticleName*Price>;;

(* makebill: Register -> Purchase -> Bill *)
exception FindArticle

let rec makeBill reg = function
    | [] -> ([],0)
    | (np,ac)::pur ->
        match Map.tryFind ac reg with
        | None -> raise FindArticle
        | Some(aname,aprice) ->
            let tprice = np*aprice
            let (infos,sumbill) = makeBill reg pur
            ((np,aname,tprice)::infos, tprice+sumbill);;

let pur = [(3,"a2"); (1,"a1")];;
type a = string;;

