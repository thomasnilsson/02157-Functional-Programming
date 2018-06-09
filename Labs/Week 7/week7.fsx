type exp = | C of int
           | BinOp of exp * string * exp
           | Id of string
           | Def of string * exp * exp;;

let rec toString exp = 
    match exp with
    | C c -> string c
    | BinOp (exp1,op,exp2) -> "(" + (toString exp1) + op + (toString exp2) + ")";;

let epx = BinOp( C(1), "+", C (9) );;
let epx2 = BinOp( C(1), "+", BinOp (C 8, "*", C 9));;
toString epx2


let rec exOp exp = 
    match exp with
    | C c -> " "
    | BinOp (exp1,op,exp2) -> (exOp exp1) + op + (exOp exp2);;

exOp epx;;
exOp epx2;;
 
let rec auxDef dset exp = 
    match exp with
    | C c -> true //if constant val then its fine
    | BinOp (exp1,op,exp2) -> (auxDef dset exp1) && (auxDef dset exp2)  //if bin operator, then make sure its valid for both exp
    | Id id -> Set.contains id dset //if includes a variable, make sure it is defined in the defSet
    | Def (var,exp1,exp2) -> (auxDef (dset.Add var) exp1) && (auxDef (dset.Add var) exp2);;  //if encounter a definition of variable, add it to the dset.

let isDef exp = auxDef Set.empty exp;;


       


(* Interpreter for a simple WHILE-language.           MRH 21/10 2013 *)
(* Program skeleton                                                  *)
(* Based on a natural semantics of WHILE                             *)

type AExp =                           (* Arithmetical expressions *) 
          | N  of int                 (* numbers                  *)
          | V  of string              (* variables                *)
          | Add of AExp * AExp        (* addition                 *)
          | Mul of AExp * AExp        (* multiplication           *)
          | Sub of AExp * AExp;;      (* subtraction              *)


type BExp =                          (* boolean expressions      *)
          | TT                       (* true                     *)
          | FF                       (* false                    *)
          | Eq of AExp * AExp                (* equality                 *)
          | Lt of AExp * AExp                (* less than                *)
          | Neg of BExp               (* negation                 *)
          | Con of BExp * BExp;;     (* conjunction              *)

type Stm  =                            (* statements             *)
          | Ass of string * AExp       (* assignment             *)
          | Skip
          | Seq  of Stm * Stm          (* sequential composition *)
          | ITE   of BExp * Stm * Stm  (* if-then-else           *)
          | While of BExp * Stm;;      (* while                  *)



type State = Map<string,int>;;

(* update: string -> int -> State -> State  *)
let update x v s = Map.add x v s;; 

(* A: AExp -> State -> int                   *)
let rec A a s      = 
   match a with 
    | N n         -> n
    | V x         -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s;;

(* B: BExp -> State -> bool                  *)
let rec B b s =
   match b with 
    | TT          -> true
    | FF          -> false
    | Eq(a1,a2) -> A a1 s = A a2
    | Lt(a1,a2) -> A a1 s < A a2 
    | Neg b -> not (B b)
    | Con(b1,b2) -> ;;

(* I: Stm -> State -> State                          *)
let rec I stm s =
    match stm with 
    | Ass(x,a)         -> update x ( ... ) s
    | Skip             -> ...
    | Seq(stm1, stm2)  -> ...
    | ITE(b,stm1,stm2) -> ...
    | While(b, stm)    -> ... ;;


(* Factorial computation 
{pre: x = K and x>=0} 
   y:=1 ; while !(x=0) do (y:= y*x;x:=x-1) 
post: {y = K!}
*)

let fac = Seq(Ass("y", N 1), 
              While(Neg(Eq(V "x", N 0)), 
                    Seq(Ass("y", Mul(V "x", V "y")) , Ass("x", Sub(V "x", N 1)) ))
             );;




(* Define an initial state                           *)
let s0 = Map.ofList [("x",4)];;

(* Interpret the program                             *)
let s1 = I fac s0;;

(* Inspect the resulting state                       *)
Map.find "y" s1;;