import Control.Monad
import Control.Applicative

data Name = Name {
  firstName :: String,
  lastName :: String
}

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman | Sophmore | Junior | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student {
  studentId :: Int,
  gradeLevel :: GradeLevel,
  studentName :: Name
} deriving Show

data Teacher = Teacher {
  teacherId :: Int,
  teacherName :: Name
} deriving Show

data Course = Course {
  courseId :: Int,
  courseTitle :: String,
  teacher :: Int
} deriving Show

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

data Enrollment = Enrollment {
  student :: Int,
  course :: Int
} deriving Show

students :: [Student]
students =  [(Student 1 Senior (Name "Audre" "Lorde")),
             (Student 2 Junior (Name "Leslie" "Silko")),
             (Student 3 Freshman (Name "Judith" "Butler")),
             (Student 4 Senior (Name "Guy" "Debord")),
             (Student 5 Sophmore (Name "Jean" "Baudrillard")),
             (Student 6 Junior (Name "Julia" "Kristeva"))]

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior"),
            Teacher 200 (Name "Susan" "Sontag")]

courses :: [Course]
courses = [Course 101 "French" 100,
           Course 201 "English" 200]

enrollments :: [Enrollment]
enrollments = [(Enrollment 1 101),
               (Enrollment 2 101),
               (Enrollment 2 201),
               (Enrollment 3 101),
               (Enrollment 4 201),
               (Enrollment 4 101),
               (Enrollment 5 101),
               (Enrollment 6 201)]

_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
  val <-vals
  return (prop val)

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
  val <- vals
  guard (test val)
  return val

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1, d2)
  guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
  return dpairs

_hinq selectQuery joinQuery whereQuery = (\joinData ->
                                            (\whereResult ->
                                              selectQuery whereResult)
                                            (whereQuery joinData)
                                         ) joinQuery


startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((== "English") .courseTitle . snd))

studentEnrollmentsQ = HINQ_ (_select (\(st,en) -> (studentName st, course en)))
                            (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ


englishStudentsQ = HINQ (_select (fst . fst))
                        (_join studentEnrollments courses snd courseId)
                        (_where ((== "English") . courseTitle . snd))
