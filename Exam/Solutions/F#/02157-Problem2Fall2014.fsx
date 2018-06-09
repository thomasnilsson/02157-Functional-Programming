(* Problem 2 *) 

(* Q1 *) 

let timesTable n = seq {for i in [1..10] do
                          yield n*i}

(* Alternative *)

let timesTable1 n = Seq.init 10 (fun i -> (i+1) *i);; 

(* Q2 *)

let tableOf n m f = seq {for i in [1..n] do 
                           for j in [1..m] do 
                                yield (i,j,f i j) }

(* Alternative *) 

let tableOf1 n m f =
   let g i = Seq.fold (fun sq j -> Seq.append sq (Seq.singleton (i,j,f i j))) Seq.empty (seq [1..m])
   Seq.collect g (seq [1..n]);;

(* Q3 *)

let aSeq = Seq.initInfinite (fun i -> String.replicate (i+1) "a");;

(* Alternative *)

let aSeq1 = seq { for j in Seq.initInfinite (fun i -> i+1) do  
                      yield (String.replicate j "a") }


(* Q4 *) 

let rec f i = function 
              | []  -> []
              | x::xs -> (x+i)::f (i*i) xs;;     


(* The type of f: int -> int list - int list *)
(* f i [x0; x1; x2; ... ; xn ] = [x0+i; x1+i^2; x2+i^4;...; xn+i^(2^n)] *)  


(* Q5 *)

let rec fA a i = function 
                 | []    -> List.rev a
                 | x::xs -> fA (x+i::a) (i*i) xs;;



let rec fC k i = function 
                 | []    -> k []
                 | x::xs -> fC (fun res -> k(x+i::res)) (i*i) xs;;


