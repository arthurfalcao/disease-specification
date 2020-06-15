import Data.Time.Clock
import Data.Time.Calendar

type Name = String

type Virus = String

type Symptoms = [String]

type Quarantine = Bool

type Date = (Integer, Int, Int)

type EndDate = Date

type Connections = [String]

type Disease = (Name, Virus, Symptoms, Quarantine)

type Patient = (Name, Symptoms, Date)

type PatientQuarantine = (Name, String, EndDate, Connections)

listDiseases :: [Disease]
listDiseases =
  [ ("hepatiteA", "picorna", ["icterícia", "fadiga", "febre", "mialgia"], False),
    ("covid_19", "corona", ["tosse", "fadiga", "febre", "dispneia"], True),
    ("sarampo", "paramyxo", ["manchas", "erup¸c~oes", "tosse", "febre"], True)
  ]

listPatients :: [Patient]
listPatients =
  [ ("Joao", ["tosse", "dispneia"], (2020, 4, 2)),
    ("Ana", ["icterícia", "mialgia"], (2020, 4, 8))
  ]

listQuarantine :: [PatientQuarantine]
listQuarantine = [("Joao", "covid_19", (2020, 5, 25), ["Maria", "Peter"])]

-- insert new disease
insertDisease :: IO ()
insertDisease = do
  putStr "Name: "
  n <- getLine
  putStr "Virus: "
  v <- getLine
  -- add symptoms
  s <- addSymptoms []
  putStr "Quarantine: "
  q <- getLine
  appendFile "data/diseases.txt" (n ++ "\t" ++ v ++ "\t" ++ join "-" s ++ "\t" ++ q ++ "\n")
  putStr "Insert another one? (y or n) "
  resp <- getLine
  if (resp == "y" || resp == "Y") then insertDisease else return ()

-- insert new patient
insertPatient :: IO ()
insertPatient = do
  putStr "Name: "
  n <- getLine
  -- add symptoms
  s <- addSymptoms []
  date <- utctDay <$> getCurrentTime
  appendFile "data/patients.txt" (n ++ "\t" ++ join "-" s ++ "\t" ++ show date ++ "\n")
  putStr "Insert another one? (y or n) "
  resp <- getLine
  if (resp == "y" || resp == "Y") then insertPatient else return ()

-- add symptons
addSymptoms :: Symptoms -> IO Symptoms
addSymptoms xs = do
  putStr "Symptom: "
  symptom <- getLine
  let symptoms = xs ++ [symptom]
  putStr "Insert another symptom? (y or n) "
  resp <- getLine
  if resp == "y" || resp == "Y"
    then addSymptoms symptoms
    else return symptoms

getDiseases [] = []
getDiseases ([name, virus, symptoms, quarantine] : xs) = (name, virus, wordsWhen (=='-') symptoms, quarantine) : (getDiseases xs)

loadDiseases = do
  diseases <- readFile "data/diseases.txt"
  return (getDiseases (map words (lines diseases)))

printDiseases [] = ""
printDiseases ((name, virus, symptoms, quarantine) : xs) = "Diseases- name= " ++ name ++ ", virus= " ++ virus ++ "\n" ++ (printDiseases xs)

getPatients [] = []
getPatients ([name, symptoms, consultDate] : xs) = (name, wordsWhen (=='-') symptoms, stringToDate $ wordsWhen (=='-') consultDate) : (getPatients xs)

loadPatients = do
  patients <- readFile "data/patients.txt"
  return (getPatients (map words (lines patients)))

getQuarantines [] = []
getQuarantines ([name, disease, endDate, connections] : xs) = (name, disease, stringToDate $ wordsWhen (=='-') endDate, wordsWhen (=='-') connections) : (getQuarantines xs)

loadQuarantines = do
  quarantines <- readFile "data/quarantines.txt"
  return (getQuarantines (map words (lines quarantines)))

-- utils
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

join sep = foldr (\ a b -> a ++ if b == "" then b else sep ++ b) ""

date :: IO Date -- :: (year, month, day)
date = getCurrentTime >>= return . toGregorian . utctDay

stringToDate :: [String] -> Date
stringToDate [y,m,d] = (read y :: Integer, read m :: Int, read d :: Int)

countQuarantinePatients [] = 0
countQuarantinePatients (x:xs) = 1 + countQuarantinePatients xs

main :: IO ()
main = do
  putStrLn "1 - Insert a new disease"
  putStrLn "2 - insert a new patient"
  putStrLn "3 - Find the patient's virus"
  putStrLn "4 - Number of patients in quarantine"
  putStrLn "5 - Current date"
  putStrLn "6 - Generate graph of all connections"
  putStr "Option: "
  resp <- getLine
  if resp == "1"
    then insertDisease
    else
      if resp == "2"
        then insertPatient
        else
          if resp == "4"
            then do
              quarantines <- loadQuarantines
              print $ countQuarantinePatients quarantines
            else error "Wrong option"
  putStr "Want to continue? "
  resp <- getLine
  if resp == "y" || resp == "Y" then main else return ()
