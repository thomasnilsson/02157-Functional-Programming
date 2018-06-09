//Exercise 4.1
let upto n = [1..n];;

//Exercise 4.2
let downto1 n = [n..(-1)..1];;

//Exercise 4.3
let rec evenN n = 
    if (n=0) then []
    else if (n%2=0) then n::evenN (n-1)
    else evenN (n-1);;


 evenN 8;;

//Exercise 4.9
let rec zip list1 list2 = 
    match (list1, list2) with
    | (_,[]) -> []
    | ([],_) -> []
    | (prefix1::rest1, prefix2::rest2) -> (prefix1, prefix2) :: (zip rest1 rest2);;

zip [1;2;3] [4;5;6];;


//Exercise 4.11.1 
 let rec count x xs = 
    match (x,xs) with
     | (x,[]) -> 0
     | (x,prefix::rest) when (prefix = x) -> 1 + (count x rest)
     | (x,prefix::rest) when (prefix < x) -> (count x rest)
     | (x,prefix::rest) -> 0;;

count 3 [1; 2; 3; 3; 4; 4; 3;];;

//Exercise 4.11.2
 let rec insert x xs = 
    match (x,xs) with
     | (x,[]) -> [x]
     | (x,prefix::rest) when (prefix < x) -> prefix :: (insert x rest)
     | (x,xs) -> x :: xs;;

    insert 3 [1;2;3];;
    insert 3 [1;2;3;4;5];;
    insert 3 [4;2;3;4];;

//Exercise 4.11.3
  let rec intersect list1 list2 =
    match (list1, list2) with
    | (list,[]) -> []
    | ([],list) -> []
    | (x::xs,y::ys) when (x = y) -> x :: (intersect xs ys)
    | (x::xs,y::ys) when (x < y) -> (intersect xs (y::ys))
    | (x::xs,y::ys) when (x > y) -> (intersect (x::xs) ys);;

    intersect [1;2;3;5] [1;2;2;3];;
    intersect [1;2;3;1] [1;2;2;3;1];;

  //Exercise 4.11.4
  let rec plus list1 list2 =
    match (list1, list2) with
    | (list,[]) -> list
    | ([],list) -> list
    | (x::xs,y::ys) when (x < y) -> x :: (plus xs (y::ys))
    | (x::xs,y::ys) when (x = y) -> x :: (y :: (plus xs ys))
    | (x::xs,y::ys) when (x > y) -> y :: (plus (x::xs) ys);;

    plus [1;2;3;4;5] [3;4;5;6;7];;

   //Exercise 4.11.4
   let rec minus list1 list2 =
    match (list1, list2) with
    | (list,[]) -> list
    | ([],list) -> []
    | (x::xs, y::ys) when (x = y) -> minus xs ys
    | (x::xs, y::ys) when (x < y) -> x :: (minus xs (y::ys))
    | (x::xs, y::ys) when (x > y) -> (minus (x::xs) ys);;

    minus [1;1;1;2;2] [1;1;2;3];;
    minus [1;1;2;3] [1;1;1;2;2];;

    let rec sum p list  =
    match (p,list) with
    | (p, []) -> 0
    | (p, x::xs) when (p x) -> x + (sum p xs);;

    let p(x) = x > 0;;

    sum p [0;1;2;3];;
