//2.1
let rec ordered = function
    | x::(y::ys) -> (x <= y) && ordered (y::ys) 
    | _ -> true;

let list1 = [1..9];;
let list2 = List.rev list1;;
ordered list1;; //true
ordered list2;; //false

//2.2
let rec smallerThanAll x xs = List.forall (fun y -> x < y) xs;;
smallerThanAll 5 list1;;
smallerThanAll 5 [6..10];;

let rec insertBefore p x xs = 
    match xs with
    | [] -> [x]
    | y::ys when (p y) -> x::(y::ys)
    | y::ys -> y::(insertBefore p x ys);;


let pred x = x > 5;;

//2.3
insertBefore pred 10 list1;;

type Sex = | M // male
           | F;; // female

//2.4
let sexToString = function
    | M -> "Male"
    | F -> "Female";;
    
sexToString M;;

//2.5
String.replicate 0 "hej ";;


//3.1
type Name = string;;
type YearOfBirth = int;;
type FamilyTree = P of Name * Sex * YearOfBirth * Children
and Children = FamilyTree list;;

let marychildren = [P("Peter",M,2005,[]);P("Bob",M,2008,[]);P("Eve",F,2010,[])];;

let joechildren = [P("Stanley",M,1975,[]);P("Mary",F,1980,marychildren);P("Jane",F,1985,[])];;
let maychildren = [P("Fred",M,1970,[]);P("Joan",F,1975,[])];;

let larrychildren = [P("May",F,1945,maychildren);P("Joe",M,1950,joechildren);P("Paul",M,1955,[])];;
let root = P("Larry",M,1920,larrychildren);;

root //prints the tree

//WELL-FORMED definition:
//1: every person must be older than his/her children.
//2: the children must appear in ascending order with regards to age.

 let rec COOK = function
     | P(_, _, yob, _)::((P(_, _, yob', _) as c2)::rest) -> (yob <= yob') && COOK (c2::rest)
     | _ -> true;;

 let rec POTC p_yob children = 
     match children with
     | [] -> true
     | P(_, _, yob, _)::rest -> (p_yob < yob) && (POTC p_yob rest);;

let rec isWF = function
     | P(_, _, yob, []) -> true
     | P(_, _, yob, children) -> (COOK children) && (POTC yob children) ;;

//valid
let children1 = [P("Alice",F,1980,[]);P("Charlie",M,1985,[])];;
let children2 = [P("Alice",F,1980,[]);P("Charlie",M,1985,[]);P("Doe",M,1990,[])];;
let root1 = P("Bob",M,1945,children1);;


isWF root1;;
isWF root;


//invalid
let children3 = [P("Alice",F,1920,[]);P("Charlie",M,1985,[])];;
let children4 = [P("Alice",F,1980,[]);P("Charlie",M,1920,[]);P("Doe",M,1990,[])];;
let root1 = P("Bob",M,1945,children3);;

COOK children3;; //should be true
COOK children4;; //should be false

//3.2
let makePerson = function
    | (nm, sex, yob) -> P(nm,sex,yob,[]);;

makePerson("bob",M,1920);;
makePerson(1920,213,1)

//3.3

let rec searchFileSys (ext, filesys) = 
    match (ext,filesys) with
    | (_, []) -> []
    | (ext,e::es) -> (searchElement (ext,e)) @ (searchFileSys(ext, es))
and searchElement (ext, element) = 
    match (ext,element) with
    | (ext, File(fn,fx)) when (fx=ext) -> [fn]
    | (ext, File(fn,fx)) when (fx <> ext) -> []
    | (ext, Dir(dn,fs)) -> searchFileSys(ext, fs);;




