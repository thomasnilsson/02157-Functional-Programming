//3.5
type Solution = 
| OneRoot of float
| TwoRoots of float*float
| NoSolution of string;;

System.Math.Sqrt(1.0);;

let solve(a:float,b:float,c:float) = 
    let d = (b*b)-4.0*a*c
    let x1 = (-b + System.Math.Sqrt(d))/ (2.0*a)
    let x2 = (-b - System.Math.Sqrt(d))/ (2.0*a)
    
    if (d<0.0) && (a=0.0) then NoSolution("no roots")
    else if (x1=x2) then OneRoot(x1)
    else TwoRoots(x1, x2);;

solve(1.0, 2.0, -3.0);;



//5.2
let reverse xs = List.fold (fun rs x -> x::rs) [] xs;; 

let revrev xs = List.fold (fun acc x -> reverse x::acc) []  xs;;
revrev [[1..3];[4..7]];;

//5.3
let rec sum p xs = 
    match (p,xs) with
    | (p,[]) -> 0
    | (p,x::xs) when (p x) -> x + sum p xs
    | (p,x::xs) -> 0 + sum p xs;;


let p1(x) = x <5;
let list1 = [1;2;3;4;5;6;7];;
sum p1 list1;;

//5.4
//areNB
let isMember x ys = List.exists (fun y -> y=x) ys;;
let areNb c1 c2 m = isMember (c1,c2) m || isMember (c2,c1) m;;

 