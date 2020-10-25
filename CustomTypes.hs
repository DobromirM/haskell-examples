type PatientName = (String, String)
type Age = Int
type Height = Int
type Weight = Int

firstName :: PatientName -> String
firstName name = fst name

lastName :: PatientName -> String
lastName name = snd name

patientInfo :: PatientName -> Age -> Height -> String
patientInfo name age height = fullName ++ " " ++ ageHeight
  where fullName = (firstName name) ++ ", " ++ (lastName name)
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type FirstName = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name first last) = first ++ " " ++ last
showName (NameWithMiddle first middle last) = first ++ " " ++ middle ++ " " ++ last

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Age
                       , weight :: Weight
                       , height :: Height
                       , bloodType :: BloodType }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: Patient
janeESmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 28 62 140 (BloodType A Neg)

jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }

patientCanDonateTo :: Patient -> Patient -> Bool
patientCanDonateTo donator receiver = canDonateTo (bloodType donator) (bloodType receiver)

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"


patientSummary :: Patient -> String
patientSummary patient = "*****************\n" ++
                         "Patient Name: " ++ showName (name patient) ++ "\n" ++
                         "Sex: " ++ showSex (sex patient) ++ "\n" ++
                         "Age: " ++ show (age patient) ++ "\n" ++
                         "Height: " ++ show (height patient) ++ "cm \n" ++
                         "Weight: " ++ show (weight patient) ++ "kg \n" ++
                         "Blood Type: " ++ showBloodType (bloodType patient) ++ "\n" ++
                         "*****************\n"
