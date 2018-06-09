 
//EXERCISE 1.1
let g = function
| n -> n + 4; 

g(5)

//EXERCISE 1.2
let h = function 
| (x,y) -> System.Math.Sqrt(x*x + y*y);

h(2.0,3.0);

//EXERCISE 1.3
fun n -> n + 4;
it 5;;

fun (x,y) -> System.Math.Sqrt(x*x + y*y);;
it (2.0,4.0);;

//EXERCISE 1.4
let rec f = function
| 0 -> 0
| n -> n + f(n-1);;

f(5);;

//EXERCISE 1.5
let rec fibo = function
| 0 -> 0
| 1 -> 1
| n -> fibo(n-1) + fibo(n-2);;

fibo 4

//EXERCISE 1.6
let rec sum = function
| (m,0) -> m
| (m,n) -> m + n + sum(m,n-1);;

sum(2,3);;

//Exericse 2.2
let rec pow1 n s = function
| 0 -> ""
| n -> s + pow1(s,n-1);;

let rec pow (s,n) = function
| (s,0) -> s
| (s,n) -> s +. pow(s,n-1);;

// Michael R. Hansen, August 31, 2011
// Program skeleton to be use when solving the exercise on polynomials

// An infix function +. for adding polynomials.
// Has the same preceedence as +.
let rec (+.) p q =
    match (p,q) with
       | (p,[])            -> p
       | ([],q)            -> q
       | (a::p',b::q')  -> (a+b) :: (p' +. q') ;;
let a = [1;2;3];;
let b = [2;4;6];;

a+.b

// multiply a polynimial by a constant
let rec multC c = function
  |  [] -> []
  |  a::p' -> (a*c) :: multC c p';;

multC 8 [1;2;3];

// multiply P(x) by x
let multX p = 0::p;;
multX a;

// An infix function *. for mytiplying polynomials
// Has the same preceedence as *

let rec ( *.) p q =
   match (p,q) with
     | (p,[])            -> []
     | ([],q)            -> []
     | (a::p',q)  -> (multC a q) +. (multX p)  ;;

a *. b;

// convert a polynomial to a string representation
// you may use an auxiliary function 
let rec toS = function
| ([], k) -> ""
| (a::p, k) -> (string (a:int)) + "x^" + (string k) + (if p=[] then "" else "+" + toS(p,k+1));;

let toString p =  toS(p,0);;



// examples
let p1 = [1; 2; 3];;

let p2 = [1; 2];;

let p3 = [1; 2; 3; 4];

let p4 = p1 +. p2 *. p3;;

let st = toString p4;;
