// Solutions to test exam questions in 02157 Functional programming
// Patrick Kasting and Michael R. Hansen 15-11 2016



// Problem 1
type Appliance = string
type Usage     = Appliance * int


// 1
// inv : Usage list -> bool
let inv ats = List.forall (fun (_,t) -> t>0) ats

// 2
// durationOf : Appliance -> Usage list -> int
let rec durationOf a = function 
                       | []                    -> 0
                       | (a',t)::ats when a=a' -> a' + durationOf a ats
                       | _::ats                -> durationOf a ats;;
                       
// Another solution:
let durationOf a ats =
    let usages = List.filter (fun (a', _) -> a' = a) ats
    List.fold (fun acc (_, t) -> acc + t) 0 usages                        

// 3
// wellFormed : Usage list -> bool
let wf aus = inv aus && List.forall (fun (a,_) -> durationOf a aus <= 24) aus

//Another solution:

// appliancesIn : Usage list -> Set<Appliance>
let appliancesOf ats =
    List.fold (fun as' (a, _) -> Set.add a as') Set.empty ats

// wellFormed : Usage list -> bool
let wellFormed ats =
    inv ats && Set.forall (fun a -> durationOf a ats <= 24) (appliancesOf ats)


// 4
// delete : Appliance * Usage list -> Usage list
let rec delete a = function 
                   | []                      -> [] 
                   | (a',_) :: ats when a=a' -> delete a ats
                   | at::ats                 -> at:: delete a ats;;

// Another solution:
let delete (a, ats) = List.filter (fun (a', _) -> a' <> a) ats


type Price  = int
type Tariff = Map<Appliance, Price> 

// 5
// isDefined : Usage list -> Tariff -> bool
let isDefined ats trf = List.forall (fun (a,_) -> Map.containsKey a trf) ats

//Another solution:
let isDefined ats trf =
    Set.forall (fun a -> Map.containsKey a trf) (appliancesOf ats)


// 6
// priceOf : Usage list -> Tariff -> int
let rec priceOf ats trf = 
   match ats with
   | []          -> 0
   | (a,t)::ats' -> match Map.tryFind a trf with
                    | Some c -> t*c + priceOf ats' trf
                    | None   -> failwith("priceOf: " + a + " is an unknown appliance")

// Another solution:
let priceOf ats trf =
    if isDefined ats trf
    then List.fold (fun sum (a, t) -> sum + (Map.find a trf) * t) 0 ats
    else failwith "All appliances are not present in the tariff."


// Problem 2 (20%)



(* mixMap f [x0; x1; ... ; xm] [y0; y1; ... ; ym] = [f(x0,y0); f(x1,y1); ...; f(xn,yn)] *) 

// mixMap : ('a * 'b -> 'c) -> 'a list -> 'b list -> 'c list
let rec mixMap f xs ys =
   match (xs,ys) with 
   | ([],[])             -> []
   | (x::xrest,y::yrest) -> f(x,y)::mixMap f xrest yrest
   | _                   -> failwith "mixMap: Lists do not have the same lengths" ;;

// Another solution:
let mixMap f xs ys = List.map f (List.zip xs ys)

   (* unMixMap f g [(x0,y0); (x1,y1); ... ; (xn,yn)] = ([f x0; f x1; ... ; f xn] , [g y0; g y1; ... ; g yn] *)


// unMixMap : ('a -> 'b) -> ('c -> 'd) -> ('a * 'c) list -> 'b list * 'd list
let rec unmixMap f g = function
   | []            -> ([], [])
   | (x,y)::xyrest -> let (fxs,gys) = unmixMap f g xyrest
                      (f x::fxs, g y::gys);; 

let unmixMap f g zs =
    let (xs, ys) = List.unzip zs in (List.map f xs, List.map g ys)


// Problem 3 (30 %)

type Tree<'a> = Lf | Br of Tree<'a> * 'a * Tree<'a>;;

// 1
// reflect : Tree<'a> -> Tree<'a>
let rec reflect = function 
                  | Lf          -> Lf
                  | Br(tl,a,tr) -> Br(reflect tr, a, reflect tl);;
   
// 2                  
// g : int -> Tree<int> -> Tree<int> * int
let rec g n = function 
              | Lf            -> (Lf, n)
              | Br(tl, a, tr) -> let anew      = a+n
                                 let (tlnew,m) = g anew tl
                                 let (trnew,l) = g m tr
                                 (Br(tlnew, anew, trnew), l)
// accumulate : Tree<int> -> Tree<int>
let accumulate t = let (t,_) = g 0 t
                   t;;
g 0 t
// Another solution:
let accumulate t =
    let rec accumulate' sum = function
        | Lf           -> (Lf, sum)
        | Br (l, x, r) ->
            let sum' = sum + x
            let (l', sum'') = accumulate' sum' l
            let (r', sum''') = accumulate' sum'' r
            (Br (l', sum', r'), sum''')
    fst (accumulate' 0 t)

let ta = Br(Br(Br(Lf,1,Lf),2, Br(Lf,3,Lf)),4, Br(Br(Lf,5,Lf),6, Br(Lf,7,Lf)));;

// 3

let rec k i t = match t with 
                | Lf          -> Lf
                | Br(tl,a,tr) -> Br(k (i*i) tl, i*a, k (i*i) tr);;                  

let rec h n m = 
   function 
   | Br(tl,a,tr) when n=m -> h n 1 tl @ [a] @ h n 1 tr
   | Br(tl,_,tr)          -> h n (m+1) tl @ h n (m+1) tr
   | Lf                   -> []
   
let q n t = h n n t;;  

// "k : int -> Tree<int> -> Tree<int>". "k i t" multiplies every node n in t
// with i^(2^d), where d is the depth of n.
// Because we provide no type information about 'i' or 'a', the multiplication
// operator '*' defaults to the type "int -> int -> int". Hence, "k : int ->
// Tree<int> -> Tree<int>" and not "k : float -> Tree<float> -> Tree<float>".

// "q : int -> Tree<'a> -> 'a list". "q n t" returns an in-order of the values
// from every n'th level of t including the root level.

    

