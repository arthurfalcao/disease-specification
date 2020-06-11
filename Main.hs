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
  putStr "Symptoms: "
  s <- getLine
  putStr "Quarantine: "
  q <- getLine
  appendFile "diseases.txt" (n ++ "\t" ++ v ++ "\t" ++ s ++ "\t" ++ q ++ "\n")
  putStr "Insert another one? (y or n) "
  resp <- getLine
  if (resp == "y" || resp == "Y") then insertDisease else return ()
