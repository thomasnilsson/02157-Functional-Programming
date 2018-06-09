//Thomas Nygaard Nilsson (s144470) & Johanne Winther Sall (s144480)
//Exercise 1
type Name = string;;
type PhoneNumber = int;;
type BirthYear = int;;
type Themes = string list;;
type Description = (PhoneNumber * BirthYear * Themes);;
type Member = (Name * Description);;
type Register = Member list;;

//Exercise 2
//unit -> unit list -> bool
//This is a borrowed function from the book, which checks if the element x is part of a given list of elements.
//x and the list must be of the same type, otherwise an error is thrown
let rec isMember x = function
| (y::ys) -> x=y || (isMember x ys)
| [] -> false;;

// (unit * int * string list) -> bool
//the first argument (no) could be of any type, since it is never used.
let p1 = function
    | (no,yb,ths) when (yb > 1982 && isMember "soccer" ths && isMember "jazz" ths) -> true
    | _ -> false;;

// (unit * int * string list) -> bool
//the first argument (no) could be of any type, since it is never used.
let p2 = function
    | (no,yb,ths) when (yb > 1982 && (isMember "soccer" ths || isMember "jazz" ths)) -> true
    | _ -> false;;

//Exercise 3
//The type for this function will be:
//(int * int * string list -> bool) -> (string * (int * int * string list)) list -> (int * string) list
let rec extractInterested p r = 
    match (p,r) with
    | (p, (name, (no, yb, ths))::reg) when (p (no, yb, ths)) -> (name, no) :: (extractInterested p reg)
    | (p, (name, (no, yb, ths))::reg) -> extractInterested p reg
    | (_, []) -> [];;

//Exercise 4
//Tests for extractInterested
let desc1 = (12345678, 1990, ["soccer";"jazz";"golf"]); 
let member1 = ("Carl", desc1);;

let desc2 = (11223344, 1980, ["soccer";"jazz";"golf"]);
let member2 = ("Sam", desc2);;

let desc3 = (13245343, 1995, ["golf";"ballet"]);
let member3 = ("Jonny", desc3);;

let desc4 = (12312312, 1995, ["golf";"ballet"; "jazz"]);
let member4 = ("Caroline", desc4);;

let reg = [member1; member2; member3; member4];;

//Should return true since Carl is the only one who is interested in both soccer and jazz AND is born after 1982.
let exp1 = [("Carl", 12345678)];;
let test1 = extractInterested p1 reg = exp1;;

//Should return false since Caroline, while being born after 1982, is only interested in jazz but not soccer.
let exp2 = [("Caroline", 12312312)];;
let test2 = extractInterested p1 reg = exp2;;

//Should return true since both Carl and Caroline are born after 1982, and have at least 1 interest that matches the requirement.
let exp3 = [("Carl", 12345678); ("Caroline", 12312312)];;
let test3 = extractInterested p2 reg=exp3;;

//Should return false since Sam is born before 1982, and thus should not belong in the result set.
let exp4 = [("Carl", 12345678); ("Sam", 11223344); ("Caroline", 12312312)];;
let test4 = extractInterested p2 reg=exp4;;

//Exercise 6
//Tests for isMember
let element1 = "element1";;
let element2 = "";;
let list1 = ["element1";"element2";"element3";"element4"];;
let list2 = ["element2";"element3";"element4"];;

//should all return true
let test5 = isMember element1 list1 = true;;
let test6 = isMember element1 list2 = false;; 
let test7 = isMember element2 list1 = false;;
let test8 = isMember element1 [] = false;;


//Tests for p1, should all return true.
let test9 = p1 desc1 = true;; //true, yb and both themes match
let test10 = p1 desc2 = false;; //false, yb<1982
let test11 = p1 desc3 = false;; //false, no matching ths
let test12 = p1 desc4 = false;; //false, yb matches, but only one theme
 
//Tests for p2, should all return true.
let test13 = p2 desc1 = true;; //true, yb matches, and one of the themes. 
let test14 = p2 desc2 = false;; //false, yb<1982
let test15 = p2 desc3 = false;; //false, no matching ths
let test16 = p2 desc4 = true;; //true, yb matches, and one of the themes.
