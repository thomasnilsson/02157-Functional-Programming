//Thomas Nygaard Nilsson s144470

type Outcome = | S | F // S: for success and F: for failure
type Sample = Outcome list
type ProbTree = | Branch of string * float * ProbTree * ProbTree
                | Leaf of string;;
//Ex1
let rec probOK ptree = 
    match ptree with
     | Leaf _ -> true
     | Branch (_,p,tl,tr) -> (p >= 0.0 && p <= 1.0) && (probOK tl) && (probOK tr);; 

//Ex2
//isSample: Outcome list -> ProbTree -> bool
let rec isSample os ptree = 
    match (os,ptree) with 
    | ([],Leaf l) -> true
    | ([], Branch (_,_,_,_)) -> false
    | (o::rest, Leaf lf) -> false
    | (S::rest, Branch (ds,p,tl,tr)) -> isSample rest tr
    | (o::rest, Branch (ds,p,tl,tr)) when o = S -> isSample rest tl;;

//Ex3
type Description = (Outcome * string) list * float * string;;

let rec auxDesc os ptree prod ods =
    match (os,ptree) with
    | ([], Leaf l) -> (ods, prod, l)
    | (o::rest, Branch (ds,p,tl,tr)) when o = F -> auxDesc rest tr (prod * (1.00 - p)) (ods @ [(o,ds)])
    | (o::rest, Branch (ds,p,tl,tr)) when o = S -> auxDesc rest tl (prod * p) (ods @ [(o,ds)])
    | _, _ -> failwith "ERROR: Not a correct sample!";;

let descriptionOf os ptree = auxDesc os ptree 1.00 (List.empty);;

//Ex4
let rec findLeaves ptree cpath =
    match ptree with
    | Leaf l -> [cpath]
    | Branch (ds,p,tl,tr) 
        -> (findLeaves tl (cpath @ [S])) 
            @ (findLeaves tr (cpath @ [F]));;


let allDescriptions ptree =  List.map (fun s -> descriptionOf s ptree) (findLeaves ptree List.empty);;
    

//Ex5
let probabilityOf (ptree: ProbTree) pred = 
    List.fold (fun acc (_,p,s) -> acc + p) 0.0 (List.filter (fun (_,_,s) -> pred s) (allDescriptions ptree));;

//Ex6
//See tests for exercise 5.

//TESTS
//Test Ex1
let valExp = Branch(">2",0.67, Branch(">3",0.5, Leaf "A", Leaf "B")
                          , Branch(">3",0.5, Leaf "C", Leaf "D"));;
let invalExp = Branch(">2",1.67, Branch(">3",0.5, Leaf "A", Leaf "B")
                          , Branch(">3",1.0, Leaf "C", Leaf "D"));;

let TestProbValid = probOK valExp = true;;
let TestProbInvalid = probOK invalExp = false;;

//Test Ex2
let valOs = [F;S];;
let invalOs = [F;F;S];;
let TestSampleValid = isSample valOs valExp = true;;
let TestSampleInvalid = isSample invalOs valExp = false;;

//Test Ex3
let TestValidDesc = descriptionOf valOs valExp;; //Example from the exam problem.
let TestInvalidDesc =  descriptionOf invalOs valExp;; //should return error message in console

//Test Ex4
let TestAllDescFullTree = allDescriptions valExp; //Example from the exam problem.
let TestAllDescLeaf = allDescriptions (Leaf "C");;

//Test Ex5
let predBC s = List.contains s ["B"; "C"];;
let TestProbabilityOfBC = probabilityOf valExp predBC;;
