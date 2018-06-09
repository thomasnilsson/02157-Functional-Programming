type Appliance = string
type Usage = Appliance * int

let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ad1; ad2; ad3; ad1; ad2]

//1.1
let inv usg = List.forall (fun (ap,t) -> t>0) usg;;

//1.2
let durationOf ap usg = 
    let filtered = List.filter (fun (ap',t) -> ap' = ap) usg
    if inv usg then List.fold (fun acc (ap',t) -> t+acc) 0 filtered
    else failwith "negative timespan error";;


//1.3
let isWF usg = List.forall (fun (ap,t) -> durationOf ap usg <= 24) usg;;

//1.4
let delete ap usg = List.filter (fun (ap',t) -> ap <> ap') usg;;

//1.5
type Price = int
type Tariff = Map<Appliance, Price>
//Usage list -> Tariff -> bool
let isDefined usg trf = 
    List.forall (fun (ap,t) -> (Map.containsKey trf ap)) usg;;

//1.6
let priceOf usg trf =
    let (aps, ts) = List.unzip usg 
    let filtered = Map.filter (fun ap price -> (List.contains ap aps)) trf
    Map.fold (fun acc ap price -> acc+price) 0 filtered;; 



//2.1
//g1: ('a -> bool) -> 'a list -> 'a list
//g1 is a filter, throwing elements away which do not satisfy p

//g2: (int -> int) i dont know what it does lul

//2.2.1 Tail recursive g1
 let rec g1A p a = function
    | x::xs when p x -> g1A p (x::a) xs
    | _ -> List.rev a;;

let pred x = x < 4

g1A pred List.empty [1..5]

//2.2.2 Continuations based g1
let rec g1C p c = function
    | x::xs when p x -> g1C p (fun v -> c (x::v)) xs 
    | _ -> c [];;

g1C pred id [1..5]

let rec repeatC s k = function
    | 0 -> k ""
    | n when n > 0 -> (repeatC s (fun v -> v + "" + k s) (n-1))
    | _ -> failwith "negative input";;

//2.3

//2.4
// f1 2 2 3 will yield: [(0,0);(0,1);(0,2);(1,0);(1,1);(2,0)]

let f1 m n k = seq { for x in [0..m] do
                        for y in [0..n] do
                            if x+y < k then
                                yield (x,y) };;


let f2 f p sq = seq { for x in sq do
                        if p x then
                            yield f x };;

let f3 g sq = seq { for s in sq do
                        yield! g s };;
//2.5
let f2S f p sq = 
    let filtered = Seq.filter (fun x -> p x) sq
    Seq.map (fun x -> f x) filtered;;

//2.6
//f1: int -> int -> int -> seq<int*int>
//computes all possible pairs of (x,y) where 0 <= x <= m, 
//0 <= y <= n and x+y < k

//f2: ('a -> 'b) -> ('a -> bool) -> seq<'a> -> seq<'b>
//for all elements in sq for which p holds, a new sequence is built
//consisting of these elements, where f is applied to each element.

//f3: ('a -> seq<'b>) -> seq<'a> -> seq<'b>
//takes in a sequence of sequences as input as well as a function g.
//maps g on all sequence in sq.

//3.1
type Name = string
type Flow = int // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries
and Tributaries = River list

let riv1 = R ("R1", 5, [])
let riv4 = R ("R4", 2, [])
let riv2 = R ("R2", 15, [riv4])
let riv3 = R ("R3", 8, [])
let riv = R ("R", 10, [riv1;riv2;riv3])

//3.2
let rec checkTrib n = function
    | [] -> false
    | (R (nm,f,t))::rest -> 
        (nm = n) || (checkTrib n rest) || (checkTrib n t);;

let contains n (R (nm,f,t)) = (n=nm) || checkTrib n t;;

//3.3
let rec g = function
    | [] -> ""
    | (R (nm,f,t))::rest -> ", " + nm + (g t) + (g rest);;

let allNames (R (nm,f,t)) = nm + (g t);;

allNames riv

//3.4
let rec tribflow = function
    | [] -> 0
    | (R (nm,f,t))::rest -> 
        f + (List.fold (fun acc (R (nm,f,t)) -> acc+f) 0 t) + (tribflow rest);;

let totalFlow (R (nm,f,t)) = f + tribflow t;;

totalFlow riv

//3.5
let rec findMax (f,n) = function
    | (R (nm,fl,t))::rest when fl > f -> findMax (findMax (fl,nm) t) rest
    | (R (nm,fl,t))::rest -> findMax (findMax (f,n) t) rest
    |  [] -> (f,n);;

let mainSource (R (nm,fl,t)) = findMax (fl,nm) t;;

mainSource riv

//3.6
let rec findRiver n r = function 
    | [] -> []
    | (R (n',f',t))::rs when n = n' -> (R (n',f',(r::t)))::rs
    | r'::rs -> r'::(findRiver n r rs);;


let tryInsert n t = function 
    | (R (n',f',t')) when (n = n') ->  Some (R (n',f',t::t'))
    | (R (n',f',t')) when (contains n (R (n',f',t'))) -> 
        let newt = findRiver n t t'
        Some (R (n',f',newt))
    | (R (n',f',t')) -> None;;

    
   
 