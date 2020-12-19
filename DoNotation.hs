import qualified Data.Map as Map

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn

-------------------------------------------------------------------------------------------

echo :: IO ()
echo = getLine >>= putStrLn

echoDo :: IO ()
echoDo = do
  line <- getLine
  putStrLn line

------------------------------------------------------------------------------------------

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate {
  candidateId :: Int,
  codeReview :: Grade,
  cultureFit :: Grade,
  education :: Degree
} deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [passedCoding, passedCultureFit, educationMin]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = do
  grade <- getLine
  return (read grade)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id:"
  cId <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "enter education:"
  degree <- readDegree
  return (Candidate { candidateId = cId
                    , codeReview = codeGrade
                    , cultureFit = cultureGrade
                    , education = degree })

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement


candidate1 :: Candidate
candidate1 = Candidate {
  candidateId = 1,
  codeReview = A,
  cultureFit = A,
  education = BA
}

candidate2 :: Candidate
candidate2 = Candidate {
  candidateId = 2,
  codeReview = C,
  cultureFit = A,
  education = PhD
}

candidate3 :: Candidate
candidate3 = Candidate {
  candidateId = 3,
  codeReview = A,
  cultureFit = B,
  education = MS
 }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1), (2,candidate2), (3,candidate3)]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

candidates :: [Candidate]
candidates = [candidate1
             ,candidate2
             ,candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

assessCandidate:: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

----------------------------------------------------------------------

type Pizza = (Double,Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
  where costP1 = costPerInch p1
        costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza " ++
                            "is cheaper at " ++
                            show costSqInch ++
                            " per square inch"
  where costSqInch = costPerInch (size,cost)

main :: IO ()
main =
  putStrLn "What is the size of pizza 1" >>
  getLine >>=
  (\size1 ->
    putStrLn "What is the cost of pizza 1" >>
    getLine >>=
    (\price1 ->
      putStrLn "What is the size of pizza 2" >>
      getLine >>=
      (\size2 ->
        putStrLn "What is the cost of pizza 2" >>
        getLine >>=
        (\price2 ->
          (\pizza1 ->
            (\pizza2 ->
              (\betterPizza ->
                putStrLn (describePizza betterPizza)
              ) (comparePizzas pizza1 pizza2)
            ) (read size2, read price2)
          ) (read size1, read price1)
        )
      )
    )
  )

costData :: Map.Map Int Double
costData = Map.fromList [(1,18.0),(2,16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.0)]

mainMonad :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
mainMonad size1 cost1 size2 cost2 = do
  s1 <- size1
  c1 <- cost1
  s2 <- size2
  c2 <- cost2
  let pizza1 = (s1,c1)
  let pizza2 = (s2,c2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
