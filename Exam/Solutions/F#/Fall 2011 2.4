type exp =  | C of int
            | BinOp of exp * string * exp
            | Id of string
            | Def of string * exp * exp

let rec search df = function
    | C i -> true
    | BinOp (e1,o,e2) -> (search df e1) && (search df e2)
    | Id s -> List.contains s df
    | Def (d,e1,e2) -> (search (d::df) e1) && (search (d::df) e2);;

let expr1 = Def("x", C 5, BinOp (Id "x", "+", Id "x"))
let expr2 = Def("x", C 5, BinOp (Id "y", "+", Id "x"))

search [] expr1;;//true
search [] expr2;;//false
search ["y"] expr2;;//true