   2*3 + 4;;

    let price = 125;;
    price *5;;
    System.Math.PI;; 
    let r = 5;;
    let circleArea p = System.Math.PI * p * p;;
    circleArea 4.8

    let pi = System.Math.PI;;

    fun r -> pi * r * r;

    it 2.0

    let circleArea = fun r -> pi* r * r;
    circleArea 2.0

    function
| 1 -> 31 // January
| 2 -> 28 // February
| 3 -> 31 // March
| 4 -> 30 // April
| 5 -> 31 // May
| 6 -> 30 // June
| 7 -> 31 // July
| 8 -> 31 // August
| 9 -> 30 // September
| 10 -> 31 // October
| 11 -> 30 // November
| 12 -> 31;;// December

it 2;

let myfunction = function
| 2 -> 28 // February
| 4 | 5 | 7 | 8 -> 30 // April
| 6 -> 30 // June
| 9 -> 30 // September
| 11 -> 30 // November
| _ -> 31;;// All other months

myfunction 5

let rec fact = function
| 0 -> 1
| n -> n * fact(n-1);;

fact 10

let rec power = function
| (x,0) -> 1.0
| (x,n) -> x * power(x,n-1);;

power(4.0,2)
power(8.0,8)

let rec gcd = function
| (a,0) -> a
| (a,b) -> gcd(b, a % b);;

let even n = n % 2 = 0;;

even(7);;

let isLowerCaseVowel ch =
ch='a' || ch='e' || ch='o';;

isLowerCaseVowel('p');

System.Char.IsLower('p');

let pok = ['a';'b';'c'];;

[1;2;3] < [3;1;2;3;5]

let rec length = function
| [] -> 0
| x::xs -> 1 + length(xs);;

length(pok);;

let z = 2::3::[4;5];;

[-3..5];;

let rec remove (x,ys) = function
match ys with
| [] -> []
| y:ys' when x=y -> remove(x,ys')
| y:ys' -> y:remove(x,ys');;


let rec altsum = function
| [] -> 0
| [x] -> x
| x0::x1::xs -> x0 - x1 + altsum xs;;

altsum(z)

let k m = function
| n -> n+m + 4; 

k(9)