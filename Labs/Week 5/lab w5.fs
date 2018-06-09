type Sex = | Male | Female;;
type Name = string;;
type PhoneNumber = int;;
type BirthYear = int;;
type Themes = Set<string>;;
type Description = PhoneNumber * Sex * BirthYear * Themes;;
type Request = Sex * BirthYear * Themes;;
type Register = Map<Name, Description>;;



let pred1 (s1,y1,t1) (_,s2,y2,t2) = (s1 <> s2) && abs(y1-y2) < 10 && not(Set.isEmpty(Set.intersect t1 t2));;

let findMatch req reg = Map.filter (fun n d -> pred1 req d) reg;;

let n1 = "børge";;
let th1 = Set.ofList ["sit overskæg"; "at være rødhåret"; "skibe"];;
let d1 = (2323232, Male, 1955, th1);;

let n2 = "johanne";;
let th2 = Set.ofList ["druk"; "fester"; "heste"; "skibe"];;
let d2 = (12423212, Female, 1955, th2);;

let newreg  = Map.ofList [(n1, d1); (n2,d2)];;

let request = (Female, 1955, th2);;
findMatch request newreg;;


   