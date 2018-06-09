type BinTree<'a> = | Leaf
                   | Node of BinTree<'a> * 'a * BinTree<'a>;;

let rec genTree xs =
    match xs with
    | [| |] -> Leaf
    | [| x |] -> Node(Leaf,x,Leaf)
    | _ ->  let m = xs.Length / 2
            let xsl = xs.[0..m-1]
            let xm = xs.[m]
            let xsr = xs.[m+1 ..]
            Node(genTree xsl, xm, genTree xsr);;

let rec count = function
    | Leaf -> 0
    | Node(tl,n,tr) -> count tl + count tr + 1;;

let tree n = genTree [| 1..n |];;

count (tree 200)