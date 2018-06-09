#time
//Biglist
let rec bigListC n c =
    if n=0 then c []
    else bigListC (n-1) (fun res -> c(res @ [n]));;

bigListC 100 id;;

let rec bigListC2 n c =
    if n=0 then c []
    else bigListC (n-1) (fun res -> c(n :: res));;


bigListC2 100 id;;

//Binary tree
type BinTree<'a> = | Leaf
                   | Node of BinTree<'a> * 'a * BinTree<'a>;;
//general solutiom
let rec count = function
    | Leaf -> 0
    | Node(tl,n,tr) -> count tl + count tr + 1;;
//using continuations
let rec countC t c =
    match t with
    | Leaf -> c 0
    | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)));;

//generate tree
let rec genTree xs =
    match xs with
    | [| |] -> Leaf
    | [| x |] -> Node(Leaf,x,Leaf)
    | _ ->  let m = xs.Length / 2
            let xsl = xs.[0..m-1]
            let xm = xs.[m]
            let xsr = xs.[m+1 ..]
            Node(genTree xsl, xm, genTree xsr);;

let t n = genTree [| 1..n |];;

let t2mil = t 20000000;;

count t2mil;;
countC t2mil id;;


//Exercise 9.6 - factorial with continuations
//tail recursive
let rec factA = function
    | (0,m) -> m
    | (n,m) -> factA(n-1,n*m);;

factA (31,1);;

//with continuations
let rec factC n c =
    if n = 0 then c 1
    else factC (n - 1) (fun m -> c(n * m));;  
let factorial n = factC n id;;

factorial 31;;

let iff x = x > 2 ? 1 : 2;;



//9.7.2 fibonacci with continuations
//naive
let rec fibo = function
| 0 -> 0
| 1 -> 1
| n -> fibo(n-1) + fibo(n-2);;

//with continuations
let rec fiboC n c = 
 match n with
 | n when n <= 1 -> c n
 | n -> fiboC (n-1) (fun a -> fiboC (n-2) (fun b -> c(a + b)));;

fiboC 2 id;;
fiboC 3 id;;
fiboC 4 id;;
fiboC 5 id;;
fiboC 6 id;;
fiboC 7 id;;
fiboC 8 id;;
fiboC 9 id;;

let rec fiboAux n c = 
    if n <= 1 then c n
    else fiboAux (n - 1) (fun a -> fiboAux (n-2) (fun b -> c(a + b)));;

let rec fiboAux c = function
    | 0 -> c 0
    | 1 -> c 1
    | n -> fiboAux (fun a -> fiboAux (n - 2) (fun b -> c(a + b))) (n - 1);;

let fibo n = fiboAux n id;;

fibo 24

//exercise 9.9 binary tree count internal nodes
let rec countAC t acc c = 
    match t with 
    | Leaf -> c acc
    | Node(tl,n,tr) -> countAC tl (acc+1) (fun v -> countAC tr v c);;  

let t10nodes = t 10;;
countAC t10nodes 0 id;;
countC t10nodes id;;
