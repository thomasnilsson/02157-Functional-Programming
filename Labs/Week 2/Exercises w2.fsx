let f  n = 
 if (n%5) = 0 then "false"
 elif (n%3) = 0 then "true"
 elif (n%2) = 0 then "true"
 else "false"


f 24
f 29
f 30

   let notDivisible d n =
    if (n%d) = 0 then false
    else true

    notDivisible 3 6
     notDivisible 10 2

   let rec test a b c = 
    if a>b then true
    else (notDivisible a c) && (test (a+1) b c)

    test 2 1 1 

    let prime n = 
        if n=1 then true
        elif n=2 then true
        else test 2 (n-1) n

    let rec nextprime n =
    if prime (n+1) then n+1
    else nextprime (n+1)

    nextprime 15

    let rec primelist n m = 
        if n > m then []
        else if (prime n) then n::primelist (n+1) m
        else primelist (n+1) m;;

    let rec firstprimes n = 
        if n=0 then []
        else if (prime n) then n::firstprimes (n-1)
        else firstprimes (n-1)
        

    let rec pascal n k =
        if n = k || n = 0 then 1
        else if k = 0 || k > n then 1
        else pascal (n-1) (k-1) + pascal (n-1) (k)

        pascal 1 0
        pascal 0 1
        pascal 0 0
        pascal 1 2
        pascal 2 1
        pascal 
    firstprimes 18
    firstprimes 19

    primelist 3 17
    prime 3
    prime 5

prime (7)        
prime 17
prime 15
prime (2)









//mergesort


//merge
let rec merge p q =
    match (p,q) with
       | (p,[]) -> p
       | ([],q) -> q
       | (a::p',b::q') -> (a) :: (b) :: (merge p' q') ;;

let merge (l : 'a list) (r : 'a list) =
    let n = l.Length + r.Length
    let res = Array.zeroCreate<'a> n
    let mutable i, j = 0, 0
    for k = 0 to n-1 do
        if i >= l.Length   then res.[k] <- r.[j]; j <- j + 1
        elif j >= r.Length then res.[k] <- l.[i]; i <- i + 1
        elif l.[i] < r.[j] then res.[k] <- l.[i]; i <- i + 1
        else res.[k] <- r.[j]; j <- j + 1

    res
     

 



    let l1 = [1;2;3] 
    let l2 = [4;5;6] 
    
    merge l1 l2
  

    
