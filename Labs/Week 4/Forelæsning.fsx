type Shape =
| Circle of float
| Square of float
| Triangle of float*float*float;;


let area = function
| Circle r -> System.Math.PI * r * r
| Square a -> a * a
| Triangle (a,b,c) -> 
    let s = (a+b+c)/2.0
    System.Math.Sqrt(s*(s-a)*(s-b)*(s*c));;

area (Circle 3.0);;


type Room = 
| Auditorium of string * int //location and nseats
| Databar of string * int * int;; //location, seats, computers


let seatCapacity = function
| Auditorium (location,nseats) -> nseats
| Databar (_,_,nseats) -> nseats;;

//seatCapacity Auditorium "hej" 120;;

//MAPPING
let rec map f = function
| [] -> []
| x::xs -> (f x)::(map f xs);;

let g = map (function x -> (x*x) + 1);;

g [2..10];;

//EXISTS (true if p(x) for some element in a list)
let isMember x ys = List.exists (fun y -> y=x) ys;;

//forall

let rec forall p = function
| [] -> true
| (x::xs) -> (p x) && (forall p xs);;



//disjoint (xs and ys must have no common element)
let disjoint xs ys = 
    forall (fun x -> not(isMember x ys)) xs;;

disjoint [2..6] [9..12];;


let subset xs ys = forall (fun x -> isMember x ys) xs;;
subset [1..3] [1..12];;
subset [1..3] [1..9];;

//filter out elements which do not satisfy p(x).
let rec filter p = function
| [] -> []
| x::xs when (p x) -> x :: filter p xs
| x::xs -> filter p xs;;

let rec tryFind p = function
| x::xs when (p x) -> Some x
| _::xs -> tryFind p xs
| _ -> None;;

//norms

let norm(x1:float,y1:float) = sqrt(x1*x1 + y1*y1;;

let rec sumOfNorms = function
| [] -> 0.0
| v::vs -> norm v + (sumOfNorms vs);;

//folding back
let rec foldBack f xList e = 
    match xList with
| (x::xs) -> f x (foldBack f xs e)
| [] -> e;;

let rec length = function
| [] -> 0
| x::xs -> 1 + length xs;;

length [2..10]

//insertion exericse

let insert x ys =  
    match (x,ys) with
| (_,[]) -> []
| (x,ys) when (isMember x ys) -> ys
| (x,ys) -> x::ys;;

let insert2 x ys = if isMember x ys then ys else x::ys;;

insert2 3 [1..6];;
insert2 7 [1..6];;

List.fold (*) 9 [1..3];;

//fold
let rec fold f e = function
| x::xs -> fold f (f e x) xs
| [] -> e;;

let reverse xs = fold (fun rs x -> x::rs) [] xs;; 
reverse [1..10];;           


