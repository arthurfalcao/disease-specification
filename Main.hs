module DiseaseSpecification where

import Control.Monad
import Data.Time.Calendar
import Data.Time.Clock
import System.IO
import Text.Printf

type Name = String

type Virus = String

type Symptoms = [String]

data Quarantine
  = Yes
  | No
  deriving (Eq, Show, Read)

type Date = (Integer, Int, Int)

type EndDate = Date

type Connections = [Name]

type Disease = (Name, Virus, Symptoms, Quarantine)

type Patient = (Name, Symptoms, Date)

type PatientQuarantine = (Name, String, EndDate, Connections)

-- insert new disease
insertDisease :: IO ()
insertDisease = do
  putStr "Name: "
  n <- getLine
  putStr "Virus: "
  v <- getLine
  s <- addSymptoms []
  putStr "Quarantine: "
  q <- getLine
  appendFile "data/diseases.txt" $ printf "%s %s %s %s\n" n v (join' "-" s) q
  putStr "Insert another one? (y or n) "
  resp <- getLine
  when (resp == "y" || resp == "Y") insertDisease

-- insert new patient
insertPatient :: IO ()
insertPatient = do
  putStr "Name: "
  n <- getLine
  -- add symptoms
  s <- addSymptoms []
  date <- utctDay <$> getCurrentTime
  diseases <- loadDiseases
  let ds = head $ map (`findDiseases` diseases) s
  if null ds
    then appendFile "data/patients.txt" $
         printf "%s %s %s\n" n (join' "-" s) (show date)
    else do
      c <- addConnections []
      let disease = getDiseaseName $ head ds
      appendFile "data/patients.txt" $
        printf "%s %s %s\n" n (join' "-" s) (show date)
      appendFile "data/quarantines.txt" $
        printf "%s %s %s %s\n" n disease (show $ addDays 40 date) (join' "-" c)
  putStr "Insert another one? (y or n) "
  resp <- getLine
  when (resp == "y" || resp == "Y") insertPatient

updateQuarantine :: IO ()
updateQuarantine = do
  date <- utctDay <$> getCurrentTime
  q <- loadQuarantines
  let expiredQuarantines =
        filter (\(n, v, d, c) -> diffDays (read d :: Day) date >= 0) q
  writeFile "data/quarantines.txt" $ save expiredQuarantines
  return ()

generateGraph :: IO ()
generateGraph = do
  q <- loadQuarantines
  let connections = map (\(n, v, d, c) -> (n, wordsWhen (== '-') c)) q
  writeFile "data/connections.dot" $ graph connections

graph list = printf "Digraph{ %s }\n" $ unwords $ map code list

code (n, c) = unwords $ map (printf "\"%s\" -> \"%s\"\n" n) c

save [] = ""
save ((n, v, d, c):xs) =
  n ++ "\t" ++ v ++ "\t" ++ d ++ "\t" ++ c ++ "\n" ++ save xs

-- add connections
addConnections :: Connections -> IO Connections
addConnections xs = do
  putStr "Add a person that you have contact with: "
  connection <- getLine
  let connections = xs ++ [connection]
  putStr "Insert another connection? (y or n) "
  resp <- getLine
  if resp == "y" || resp == "Y"
    then addConnections connections
    else return connections

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
getDiseases ([name, virus, symptoms, quarantine]:xs) =
  (name, virus, wordsWhen (== '-') symptoms, read quarantine :: Quarantine) :
  getDiseases xs

loadDiseases = do
  diseases <- readFile "data/diseases.txt"
  return $ getDiseases $ map words $ lines diseases

printDiseases [] = ""
printDiseases ((name, virus, symptoms, quarantine):xs) =
  "Diseases- name= " ++ name ++ ", virus= " ++ virus ++ "\n" ++ printDiseases xs

getPatients [] = []
getPatients ([name, symptoms, consultDate]:xs) =
  ( name
  , wordsWhen (== '-') symptoms
  , stringToDate $ wordsWhen (== '-') consultDate) :
  getPatients xs

loadPatients = do
  patients <- readFile "data/patients.txt"
  return $ getPatients $ map words $ lines patients

getQuarantines [] = []
getQuarantines ([name, disease, endDate, connections]:xs) =
  (name, disease, endDate, connections) : getQuarantines xs

loadQuarantines = do
  quarantines <- readFile' "data/quarantines.txt"
  return $ getQuarantines $ map words $ lines quarantines

findPatients :: Name -> [Patient] -> [Patient]
findPatients p [] = []
findPatients p xs = filter (\(n, s, c) -> n == p) xs

findDiseases :: String -> [Disease] -> [Disease]
findDiseases s = filter (\(n, v, x, q) -> s `elem` x && q == Yes)

getDiseaseName :: Disease -> Name
getDiseaseName (n, v, s, q) = n

getDiseaseVirus :: Disease -> Virus
getDiseaseVirus (n, v, s, q) = v

getPatientSymptoms :: Patient -> Symptoms
getPatientSymptoms (n, s, d) = s

-- utils
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

join' sep =
  foldr
    (\a b ->
       a ++
       if b == ""
         then b
         else sep ++ b)
    ""

date :: IO Date -- :: (year, month, day)
date = toGregorian . utctDay <$> getCurrentTime

stringToDate :: [String] -> Date
stringToDate [y, m, d] = (read y :: Integer, read m :: Int, read d :: Int)

readFile' filename =
  withFile filename ReadMode $ \handle -> do
    theContent <- hGetContents handle
    mapM return theContent

countQuarantinePatients [] = 0
countQuarantinePatients xs = foldr (\x -> (+) 1) 0 xs

main :: IO ()
main = do
  putStrLn "1 - Insert a new disease"
  putStrLn "2 - Insert a new patient"
  putStrLn "3 - Find the patient's virus"
  putStrLn "4 - Number of patients in quarantine"
  putStrLn "5 - Update the quarantine by the current date"
  putStrLn "6 - Generate graph of all connections"
  putStr "Option: "
  resp <- getLine
  if resp == "1"
    then insertDisease
    else if resp == "2"
           then insertPatient
           else if resp == "3"
                  then do
                    putStr "Patient name: "
                    patientName <- getLine
                    allPatients <- loadPatients
                    let patients = findPatients patientName allPatients
                    if null patients
                      then putStrLn "Patient not found"
                      else do
                        diseases <- loadDiseases
                        let symptoms = getPatientSymptoms $ head patients
                        let ds = map (`findDiseases` diseases) symptoms
                        if null ds
                          then error "Disease not found"
                          else print $ getDiseaseVirus $ head $ head ds
                  else if resp == "4"
                         then do
                           quarantines <- loadQuarantines
                           print $ countQuarantinePatients quarantines
                         else if resp == "5"
                                then updateQuarantine
                                else if resp == "6"
                                       then generateGraph
                                       else error "Wrong option"
  putStr "Want to continue? "
  resp <- getLine
  when (resp == "y" || resp == "Y") main
