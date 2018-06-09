//1.1
let rec repeat s = function
    | 0 -> ""
    | n when n > 0 -> s + (repeat s (n-1))
    | _ -> failwith "negative input";;

//1.2
let f s1 s2 = function
    | 0 -> ""
    | n when (n > 0 && n%2=0) -> 
        repeat (s1 + "\n" + s2 + "\n") (n/2)
    | n when (n > 0 && n%2=1) -> 
        (repeat (s1 + "\n" + s2 + "\n") (n/2)) + s1
    | _ -> failwith "negative input";;

f "XO" "OX" 3

//1.3
let viz m n = 
    if (m > 0 && n > 0) then f (repeat "XO" m) (repeat "OX" m) n
    else failwith "something went wrong";;

"\n" + viz 5 6;;

//1.4.1 - tail recursive
let rec repeatA s a = function
    | 0 -> a
    | n when n > 0 -> (repeatA s (s + "" + a) (n-1))
    | _ -> failwith "negative input";;

repeatA "abc" "" 5

//1.4.2 - continuation
let rec repeatC s k = function
    | 0 -> k ""
    | n when n > 0 -> (repeatC s (fun v -> v + "" + k s) (n-1))
    | _ -> failwith "negative input";;

repeatC "abc" id 5

//2.1
let mixMap f xs ys = List.map f (List.zip xs ys);;

//2.2
let unMixMax f g lst = 
    let (xs,ys) = List.unzip lst
    (f xs g ys);;

//('a * 'b ->  'c) -> 'a list -> 'b list -> 'c list
//('a -> 'b) -> ('c -> 'd) -> ('a * 'c) list -> ('b list) * ('d list)

//3.1
type Tree<'a> = Lf | Br of Tree<'a> * 'a * Tree<'a>;;
let t = Br(Br(Br(Lf,1,Lf),2,Br(Lf,3,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf)));;

let rec reflect = function
    | Lf -> Lf
    | Br (tl,n,tr) -> Br ((reflect tr), n, (reflect tl));;

reflect t

//3.2
let rec travel a = function
    | Lf -> Lf,a
    | Br (tl,n,tr) ->   let anew = a+n
                        let (tlnew, l) = travel anew tl
                        let (trnew, m) = travel l tr
                        Br (tlnew, m, trnew), m;;

let accumulate tree = 
    let (res, acc) = travel 0 tree
    res;;

accumulate t

//3.3
//k: 'a -> Tree<'a> -> Tree<'a>
//'a can be either int or float.
//produces a new tree where the number of each node
//is multiplied by i^d, where d is the depth of the node.


//q: int -> T<'a> -> 'a list
//q uses h to create a list of all the nodes in the input tree,
//traversing the tree in-order.

let rec k i tree =
    match tree with
    | Lf -> Lf
    | Br(tl,a,tr) -> Br(k (i*i) tl, i*a, k (i*i) tr);;

let rec h n m =
    function
    | Br(tl,a,tr) when n=m -> h n 1 tl @ [a] @ h n 1 tr
    | Br(tl,_,tr) -> h n (m+1) tl @ h n (m+1) tr
    | Lf -> []

let q n t = h n n t;;

k 2 t
q 2 t



//4.1
type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS
type CourseBase = Map<CourseNo, CourseDesc>

let isValidCourseDesc = function 
    | (cn, ects)-> (ects%5 = 0) && (ects >= 5);
    | _ -> false;;

//4.2
let isValidCourseBase cb = Map.forall (fun cn cd -> isValidCourseDesc cd) cb;;

//4.3
let disjoint s1 s2 = Set.isEmpty (Set.intersect s1 s2);;

//4.4
let sumECTS cs cb = 
    let found = Map.filter (fun cn cd -> Set.contains cs cn) cb
    Map.fold (fun acc cn (title,ects) -> acc+ects) 0 found;;

//4.5
type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional

let isValidCourseGroup cb (man,opt) = 
    let manECTS = sumECTS man cb
    let optECTS = sumECTS opt cb
    (disjoint man opt) &&
    (manECTS <= 45) &&
    (manECTS = 45 && Set.isEmpty opt) &&
    (manECTS + optECTS) >= 45;; 

//4.6
type BasicNaturalScience = CourseGroup
type TechnologicalCore = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective = CourseNo -> bool
type FlagModel = BasicNaturalScience * TechnologicalCore
                    * ProjectProfessionalSkill * Elective
type CoursePlan = Set<CourseNo>

let CG_Disjoint bns tc pps = 
    let bnsUnion = Set.union (fst bns) (snd bns)
    let tcUnion =  Set.union (fst tc) (snd tc)
    let ppsUnion =  Set.union (fst pps) (snd pps)
    (disjoint bnsUnion tcUnion) &&
    (disjoint bnsUnion ppsUnion) &&
    (disjoint tcUnion ppsUnion);;

let All_Elective bns tc pps ep =
    Set.forall ep (fst bns) && Set.forall ep (snd bns) &&
    Set.forall ep (fst tc) && Set.forall ep (snd tc) &&
    Set.forall ep (fst pps) && Set.forall ep (snd pps);;
    
let isValid (bns,tc,pps,ep) cb =
    isValidCourseGroup cb bns &&
    isValidCourseGroup cb tc &&
    isValidCourseGroup cb pps &&
    CG_Disjoint bns tc pps &&
    All_Elective bns tc pps ep;;

//4.7
let checkPlan cp fm cb =
    isValid fm cb && //isvalid already checks than all courses are elective, and that each courseset is at least 45 points
    (sumECTS cp cb = 180);;

    