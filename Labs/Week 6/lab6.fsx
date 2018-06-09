type FileName = string;;
type FileExtension = string;;
type DirName = string;;

type FileSys = Element list
and Element = | File of FileName * FileExtension
              | Dir of DirName * FileSys;;


let rec namesFileSys = function
        | [] -> []
        | e::es -> (namesElement e) @ (namesFileSys es)
and namesElement = function
        | File (fn,fx) -> [(fn + "." + fx)]
        | Dir(dn,fs) -> dn :: (namesFileSys fs);;

let d1 = Dir("d1",[File("a1","java");
        Dir("d2", [File("a2","fsx");
            Dir("d3", [File("a3","fs")])]);
        File("a4","fsx");
        Dir("d3", [File("a5","pdf")])]);;

namesElement d1;;

let rec searchFileSys (ext, filesys) = 
    match (ext,filesys) with
    | (_, []) -> []
    | (ext,e::es) -> (searchElement (ext,e)) @ (searchFileSys(ext, es))
and searchElement (ext, element) = 
    match (ext,element) with
    | (ext, File(fn,fx)) when (fx=ext) -> [fn]
    | (ext, File(fn,fx)) when (fx <> ext) -> []
    | (ext, Dir(dn,fs)) -> searchFileSys(ext, fs);;

searchElement ("fsx", d1);;

let rec longNamesFileSys path = function
        | [] -> []
        | e::es -> (longNamesElement path e) @ (longNamesFileSys path es)

and longNamesElement path = function
        | File (fn,fx) -> [(path + fn + "." + fx)]
        | Dir(dn,fs) -> (longNamesFileSys (path + dn + "\\") fs);;

longNamesElement "root\\" d1;;

//6.2

type BinTree<'a> = | Leaf
                   | Node of BinTree<'a> * 'a * BinTree<'a>;;

type ExprTree = | Const of int
                | Ident of string
                | Minus of ExprTree
                | Sum of ExprTree * ExprTree
                | Diff of ExprTree * ExprTree
                | Prod of ExprTree * ExprTree
                | Let of string * ExprTree * ExprTree;;

let rec postOrder = function
    | Leaf -> []
    | Node(tl,x,tr) -> (postOrder tl) @ (postOrder tr) @ [x];;

let et =
    Prod(Ident "a",
        Sum(Minus (Const 3),
            Let("x", Const 5, Sum(Ident "x", Ident "a"))));;

postOrder et;;
