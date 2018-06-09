//Course 02157 
//Thomas Nygaard Nilsson (S144470)

//###Types###
type CourseNo   = int
type Title      = string
type ECTS       = int
type CourseDesc = Title * ECTS 

type CourseBase = Map<CourseNo, CourseDesc>

type Mandatory   = Set<CourseNo>
type Optional    = Set<CourseNo> 
type CourseGroup = Mandatory * Optional

type BasicNaturalScience      = CourseGroup
type TechnologicalCore        = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective                 = CourseNo -> bool

type FlagModel  = BasicNaturalScience * TechnologicalCore * ProjectProfessionalSkill * Elective                 
type CoursePlan = Set<CourseNo>   


//Exercise 1
//Check that ECTS is divisible by 5. It should however not be less than 5.
let isValidCourseDesc (title,ects) = 
    (ects > 0) && (ects % 5 = 0);;

//Exercise 2
//Check that all elements in cbase satisfy isValidCourseDesc
let isValidCoursebase cbase = 
    Map.forall (fun cno cdesc -> isValidCourseDesc cdesc) cbase;;

//Exercise 3
//Check that the intersection between two sets is empty,
//this means they are disjoint.
let disjoint set1 set2 = 
    Set.isEmpty(Set.intersect set1 set2);;

//Exercise 4
//Filter out any course from the CoursePlan not found in the CourseBase.
let filterCourses cs cb = 
    Map.filter (fun cno cdesc -> Set.contains cno cs) cb;;

//Sum ECTS from all courses in CoursePlan, that are found in CourseBase.
//This is done by only folding on the mutual courses.
let sumECTS cs cb = 
    Map.fold (fun sum cno (title,ects) -> sum + ects) 0 (filterCourses cs cb);;


//Exercise 5       
//Aux function to ensure either:
//The set of Optional Courses is empty if mandatory ECTS are = 45.
//The set of Optional Courses is not empty, if mandatory ECTS are < 45.
let isDistributionValid man opt cb = 
    ((Set.isEmpty opt) && (sumECTS man cb) = 45) 
    || (not(Set.isEmpty opt) && (sumECTS man cb) < 45);;

//The Main function.
let isValidCourseGroup (man,opt) cb = 
    (disjoint man opt) //check that the two cgs are disjoint.
    && (sumECTS man cb) <= 45 //check that the mandatory is max 45.
    && (isDistributionValid man opt cb) //check that the distribution of ects is valid.
    && (sumECTS man cb + sumECTS opt cb) >= 45;; //check that total ects of cg is at least 45.


//Exercise 6
//Aux function for validity of the three CourseGroups.
let areCGsValid bns tc pps cb = 
    (isValidCourseGroup bns cb) //ensures that bns is a valid cg.
    && (isValidCourseGroup tc cb) //ensures that tc is a valid cg.
    && (isValidCourseGroup pps cb);; //ensures that pps is a valid cg.

//Aux function to check if two coursegroups are disjoint.
let areCGsDisjoint (set1,set2) (set3,set4) = 
    (disjoint (Set.union set1 set2) (Set.union set3 set4));;

//Aux function to check if the all three course groups are disjoint with each other.
let areAllDisjoint bns tc pps = 
    (areCGsDisjoint bns tc) 
    && (areCGsDisjoint bns pps) 
    && (areCGsDisjoint tc pps);; 

//Aux function for checking is coursegroup is elective
let isCGElective (man,opt) ep = 
   Set.forall (fun mancourse -> (ep mancourse)) man
   && Set.forall (fun optcourse -> (ep optcourse)) opt;;

//Aux function to check if both man and opt courses can be elected.
let areCoursesElective (bns,tc,pps,ep) = 
  (isCGElective bns ep)
  && (isCGElective tc ep)
  && (isCGElective pps ep);;

//The main function to check whether or not a FlagModel, given a CourseBase cb, is valid.
//Returns the truth value of the conjunction of all three auxillary functions:
let isValid (bns,tc,pps,ep) cb = 
    (areCGsValid bns tc pps cb) 
    && (areAllDisjoint bns tc pps) 
    && (areCoursesElective (bns,tc,pps,ep));;

//Exercise 7
//Aux function to calculate the ECTS sum of a CoursePlan, 
//given a CourseGroup in a Flagmodel, and a CourseBase.
let courseGrpECTS cs (man,opt) cb =
    sumECTS (Set.intersect cs man) cb
    + sumECTS (Set.intersect cs opt) cb;;


//Main function.
let checkPlan cs (bns,tc,pps,ep) cb = 
    (isValid (bns,tc,pps,ep) cb) //flagmodel must be valid
    && (sumECTS cs cb) = 180 //sum of ECTS must be 180
    && (courseGrpECTS cs bns cb) >= 45 //check ECTS for bns coursegroup
    && (courseGrpECTS cs tc cb) >= 45 //check ECTS for tc coursegroup
    && (courseGrpECTS cs pps cb) >= 45 //check ECTS for pps coursegroup

//TESTS
//All tests should return true when run.

//1 - Valid ECTS points.
let cd1 = ("Course 1", 0);;
let cd2 = ("Course 2", 5);;
let cd3 = ("Course 3", 6);;
let cd4 = ("Course 4", 15);;

let TestECTS1 = isValidCourseDesc cd1 = false;; //false since ects =  zero
let TestECTS2 = isValidCourseDesc cd2 = true;; //true since 5 is divisble by 5
let TestECTS3 = isValidCourseDesc cd3 = false;; //false since 6 isnt divisible by 5
let TestECTS4 = isValidCourseDesc cd4 = true;;// true since 15 is divisible by 5

//2 - Valid coursebase.
let cb1 = Map.ofList [(02157, cd1);(02158, cd2);(02159, cd3);(02160, cd4)];;
let cb2 = Map.ofList [(02158, cd2);(02160, cd4)];;

let TestCB1 = isValidCoursebase cb1 = false;; //false since contains invalid courses 
let TestCB2 = isValidCoursebase cb2 = true;; //true since no invalid courses.

//3 - Disjoint sets.
let s1 = Set.ofList [1;2;3;4];;
let s2 = Set.ofList [4;5;6;7];;
let s3 = Set.ofList [5;6;7;8];;

let TestDisjoint1 = disjoint s1 s2 = false;; //false since intersection = {4}
let TestDisjoint2 = disjoint s2 s3 = false;; //false since intersection = {5,6,7}
let TestDisjoint3 = disjoint s1 s3 = true;; //true since intersection = {}

//4 ECTS points
let cs1 = Set.ofList [02158;02160;02161];
let cs2 = Set.remove (02158) cs1;;
let TestSumECTS1 = sumECTS cs1 cb1 = 20;; //5+15+0=20, since last course doesnt exist
let TestSumECTS2 = sumECTS cs2 cb1 = 15;; //15+0=15, since last course doesnt exist


//5 Coursegroups
let cset1 = Set.ofList [02157;02158;02159];;
let cset2 = Set.ofList [00001;00002;00003];;
let cset3 = Set.ofList [02157;02159];;
let cset4 = Set.remove (00000) (Set.ofList [00000]);; //empty set, since course doesnt exist
let cset5 = Set.ofList [00001;00002];; //45 point coursegroup

let cd5 = ("Course 5", 5);;
let cd6 = ("Course 6", 10);;
let cd7 = ("Course 7", 15);;
let cd8 = ("Course 8", 20);;
let cd9 = ("Course 9", 25);;
let cd10 = ("Course 10", 30);;

let cb3 = Map.ofList [(02157, cd5);(02158, cd6);(02159, cd7);(00001, cd8);(00002, cd9);(00003, cd10)];;


let TestValidCG1 = isValidCourseGroup (cset1,cset2) cb3 = true;; //true since: disjoint, man ects < 45, total ects > 45.
let TestValidCG2 = isValidCourseGroup (cset2,cset1) cb3 = false;; //false since: man ects > 45
let TestValidCG3 = isValidCourseGroup (cset5,cset4) cb3 = true;; //true since: disjoint, man ects = 45, opt empty, total ects = 45.
let TestValidCG4 = isValidCourseGroup (cset1,cset3) cb3 = false;; //false since not disjoint!

//6 Flagmodels
let ePred cno = (cno < 3000) && (cno >= 2000);; //checks if course is a dtu compute course. 

let cd11 = ("Course 11", 30);;
let cd12 = ("Course 12", 30);;
let cd13 = ("Course 13", 30);;
let cd14 = ("Course 14", 30);;
let cd15 = ("Course 15", 30);;
let cd16 = ("Course 16", 30);;
let cd17 = ("Course 16", 30);;

let cb4 = Map.ofList [(02011, cd11);(02012, cd12);(02013, cd13);(02014, cd14);(02015, cd15);(02016, cd16);(02017, cd17)];;

//Valid CG's with man=30 and opt=30, total of 60 ects.
let bnsgrp = (Set.ofList [02011] , Set.ofList [02014]);; 
let tcgrp = (Set.ofList [02012] , Set.ofList [02015]);;
let ppsgrp = (Set.ofList [02013] , Set.ofList [02016]);;
let fmodel = (bnsgrp,tcgrp,ppsgrp,ePred);;

//if either of these return false, then the flag model is invalid,
//since isValid is the conjunction of 3 aux functions.
let TestValidCG5 = areCGsValid bnsgrp tcgrp ppsgrp cb4 = true;;
let TestDisjoint4 = areAllDisjoint bnsgrp tcgrp ppsgrp = true;;
let TestElective1 = areCoursesElective fmodel = true;;

let TestFlagModel1 = isValid fmodel cb4 = true;;

let badfmodel = ((cset1,cset2),tcgrp,ppsgrp,ePred);;
let TestFlagModel2 = isValid badfmodel cb4 = false;;

//7 CoursePlan checker
let cplan180 = Set.ofList [02011;02012;02013;02014;02015;02016];;
let cplanGT180 = Set.ofList [02011;02012;02013;02014;02015;02016;02017];;
let cplanLT180 = Set.ofList [02011;02012;02013;02014];;

//using the invalid flagmodel from previous exercise.
let TestPlanInvalidFM = checkPlan cplan180 badfmodel cb4 = false;;

//using a valid flagmodel, but courseplan containing
//less than 180 ects points.
let TestPlanLess180Points = checkPlan cplanLT180 badfmodel cb4 = false;;

//using a valid flagmodel, but courseplan containing
//more than 180 ects points.
let TestPlanMore180Points = checkPlan cplanGT180 badfmodel cb4 = false;;

//using a valid flagmodel, and courseplan contains
//exactly 180 points.
let TestPlanAllGood = checkPlan cplan180 fmodel cb4 = true;;
