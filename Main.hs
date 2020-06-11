type Name = String

type Virus = String

type Symptoms = [String]

type Quarantine = Bool

type Date = (Int, Int, Int)

type EndDate = Date

type Connections = [String]

type Disease = (Name, Virus, Symptoms, Quarantine)

listDiseases :: [Disease]
listDiseases =
  [ ("hepatiteA", "picorna", ["icterícia", "fadiga", "febre", "mialgia"], False),
    ("covid_19", "corona", ["tosse", "fadiga", "febre", "dispneia"], True),
    ("sarampo", "paramyxo", ["manchas", "erup¸c~oes", "tosse", "febre"], True)
  ]

type Patient = (Name, Symptoms, Date)

listPatients :: [Patient]
listPatients =
  [ ("Joao", ["tosse", "dispneia"], (2020, 4, 2)),
    ("Ana", ["icterícia", "mialgia"], (2020, 4, 8))
  ]

type PatientQuarantine = (Name, String, EndDate, Connections)

listQuarantine :: [PatientQuarantine]
listQuarantine = [("Joao", "covid_19", (2020, 5, 25), ["Maria", "Peter"])]
