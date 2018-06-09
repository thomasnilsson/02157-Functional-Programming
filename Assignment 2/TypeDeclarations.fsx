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

type FlagModel  = BasicNaturalScience * TechnologicalCore 
                   * ProjectProfessionalSkill * Elective                 
type CoursePlan = Set<CourseNo>   

  