type Name = String

type Virus = String

type Symptoms = [String]

type Quarantine = Bool

type Date = (Int, Int, Int)

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
  appendFile "data/diseases.txt" (n ++ "\t" ++ v ++ "\t" ++ join "," s ++ "\t" ++ q ++ "\n")
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
  putStr "Consult date: "
  d <- getLine
  appendFile "data/patients.txt" (n ++ "\t" ++ join "," s ++ "\t" ++ d ++ "\n")
  putStr "Insert another one? (y or n) "
  resp <- getLine
  if (resp == "y" || resp == "Y") then insertPatient else return ()

-- add symptons
addSymptoms :: Symptoms -> IO Symptoms
addSymptoms list = do
  putStr "Symptom: "
  s <- getLine
  let x = s : list
  putStr "Insert another symptom? (y or n) "
  resp <- getLine
  if resp == "y" || resp == "Y"
    then addSymptoms x
    else return x

join sep = foldr (\ a b -> a ++ if b == "" then b else sep ++ b) ""
