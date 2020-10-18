import Data.List

-- Even

inc n = n + 1
double n = n * 2
square n = n * n
cube n = n ^ 3

ifEven myFunc x = if even x
                  then myFunc x
                  else x

ifEvenInc x = ifEven inc x
ifEvenDouble x = ifEven double x
ifEvenSquare x = ifEven square x
ifEvenCube x = ifEven cube x

-- Sorting

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                    then LT
                                    else compareFirstNames name1 name2
  where lastName1 = snd name1
        lastName2 = snd name2


compareFirstNames name1 name2 = compare firstName1 firstName2
  where firstName1 = fst name1
        firstName2 = fst name2


names = [("Ian", "Curtis"),
         ("Bernard","Curtis"),
         ("Peter", "Hook"),
         ("Stephen","Morris")]

-- letters

sfOffice name = if lastName < "L"
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

dcOffice name = nameText ++ " PO Box 1337 - Washington DC, 20001"
  where nameText = (fst name) ++ " " ++ (snd name) ++ ", Esq."

getLocationFunction location = case location of
                               "ny" -> nyOffice
                               "sf" -> sfOffice
                               "reno" -> renoOffice
                               "dc" -> dcOffice
                               _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location