module Chapter2.Test where 

import Chapter2.DataTypes


timeMachine1 :: TimeMachine
timeMachine1 = TimeMachine "Taco Bell" 1 "Taco Time Crunch" PastFuture 3000.0
timeMachine2 :: TimeMachine
timeMachine2 = TimeMachine "Sonic Inc" 99 "Sonic Swirl" Present 99999.99
timeMachine3 :: TimeMachine
timeMachine3 = TimeMachine "US Government" 100 "Reset World" PastFuture 0 
timeMachine4 :: TimeMachine
timeMachine4 = TimeMachine "Pepsi Co" 6896 "Bringing Love on Pepsi at a time" Future 1.99

timeMachines :: [TimeMachine]
timeMachines = timeMachine1 : timeMachine2 : timeMachine3 : timeMachine4 : []


company1 :: Client
company1 = Company "Nasa" 1920 (Person "Jason" "Freddie" Male) "Slayer"

company2 :: Client
company2 = Company "US Government" 1987 (Person "George" "Bush" Male) "Also a Slayer"

company3 :: Client
company3 = Company "Ericsson" 1232 (Person "Fredrik" "Ericsson" Male) "Slayer"

individual1 :: Client
individual1 = Individual (Person "Jimmy" "Doe" Male) True 

individual2 :: Client
individual2 = Individual (Person "James" "Franco" Male) False

individual3 :: Client
individual3 = Individual (Person "Raja" "Mohemi" Female) True 

govOrg1 :: Client
govOrg1 = GovOrg "Nasa"

govOrg2 :: Client
govOrg2 = GovOrg "US Department Of Veterans Affairs"

clientList :: [Client]
clientList = company1 : company2 : company3 : individual1 : individual2 : individual3 : govOrg1 : govOrg2 : []