(* Solution to Problem 1 from exam winter 2013 in 02157        Michael R. Hansen 
                                                                      07-10-2015 *)  

type Multiset<'a when 'a : equality> = ('a * int) list;;

(* Q1 *)
let rec inv = function
              | [] -> true
              | (e,k)::m -> k>0 && List.forall (fun (e',_) -> e <> e') m && inv m;;

let rec inv' es = function 
                  | []                                       -> true
                  | (e,k)::_ when k<=0 || Set.contains e es  -> false
                  | (e,_)::ms                                -> inv' (Set.add e es) ms;;

let inv1 ms = inv' Set.empty ms;;                   
      
let ms1 = [("b",3); ("a",5); ("d",1)];;


(* Q2 *)
let rec insert e k m = 
   if k <= 0 then failwith "multiset: argument error"
   else match m with 
        | []                    -> [(e,k)]
        | (e',k')::m' when e=e' -> (e,k+k')::m'
        | (e',k')::m'           -> (e',k')::insert e k m';;


(* Q3 *)
let rec numberOf e = function
                     | []                  -> 0
                     | (e',k)::m when e=e' -> k
                     | _::m'               -> numberOf e m';;

(* Q4 *)
let rec delete e = function
                   | [] -> []
                   | (e',1)::m when e=e' -> m
                   | (e',k)::m when e=e' -> (e,k-1)::m
                   | (e',k)::m           -> (e',k)::delete e m;;


(* Q5 *)

let rec union(m, m2) = 
   match m with
   | []        -> m2
   | (e,k)::m1 -> union(m1, insert e k m2);;

let union1(m1, m2) = List.foldBack (fun (e,k) m -> insert e k m)  m1 m2;;
 
let mu =  union ([("b",3); ("a",5); ("d",1)], [("a",3); ("b",4); ("c",2)]);; 

(* Q6 *)

type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;

let inv2 m = Map.forall (fun _ n -> n>0) m;;    

let insert1 e k m = 
    if k<=0 then failwith "insert1: argument error"
    else match Map.tryFind e m with  
         | None    -> Map.add e k m
         | Some k' -> Map.add e (k+k') m;;

let union2(m1,m2) = Map.foldBack insert1 m1 m2;;

let mu1 =  union2(Map.ofList[("b",3); ("a",5); ("d",1)], Map.ofList [("a",3); ("b",4); ("c",2)]);;

                                                                   