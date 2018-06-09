#time
#time
//9.3 iterative summing function
//naive version
let rec sum = function
| (m,0) -> m
| (m,n) -> m + n + sum(m,n-1);;

//iterative version
let rec sumA c = function
| (m,0) -> c+m
| (m,n) -> sumA (c+m+n) (m,n-1);;

sum (9,8);; 
sumA 0 (9,8);; 

//9.4 iterative list length
//naive version
let rec listLength = function
| [] -> 0
| x::xs -> 1 + listLength(xs);;

let rec listLengthA = function
| (length, []) -> length
| (length, x::xs) -> listLengthA(length+1, xs);;


listLength [1..200000];;
listLengthA (0,[1..200000]);;
List.length [1..200000];;

//9.7.1 Fibonacci
//naive
let rec fibo = function
| 0 -> 0
| 1 -> 1
| n -> fibo(n-1) + fibo(n-2);;

let rec fiboMain = function //a = current fib number, b = previous fib number
| (a, b, 0) -> a         
| (a, b, n) -> fiboMain(a+b, a, n-1);;

let rec fiboA(n) = fiboMain (0,1,n);;

fibo 40
fiboA(40)

//9.8 binary tree count nodes
type Tree = | Branch of Tree * Tree
            | Leaf of string;;

let rec countNodes (tree: Tree) = 
    match tree with
    | (Leaf leaf) -> 1
    | (Tree left, Tree right) -> 1 + countNodes(left) + countNodes(right);;


