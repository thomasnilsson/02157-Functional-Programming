Seq.initInfinite (fun i -> i*i);;
seq [10; 7; -25];;
Seq.cache [1;2;3];;
let cons x sq = Seq.append (Seq.singleton x) sq;;
let rec from i = cons i (from(i+1));;
let rec from1 i = cons i (Seq.delay (fun () -> from1(i+1)));;
let rec from2 i = Seq.delay (fun () -> cons i (from2(i+1)));;

//sequence of odd numbers
let seqinf = Seq.initInfinite (fun i -> i);;
Seq.filter (fun v -> v%2=1) seqinf;;

let rec factC n c =
    if n=0 then c 1
    else factC (n-1) (fun res -> c(n * res));;

//sequence of n!
Seq.initInfinite (fun i -> factC i id);;

//sequence of n! 2.0
let rec fact a i = seq {yield a 
                        yield! fact (a*i) (i+1)}

fact 1 1

//sequnce of 0, -1, 1, -2, 2...
let seqnew a b = seq {yield (-1 * a) 
                      yield! b}

seqnew 1 []

2+1
    

