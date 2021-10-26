module Chapter2.PatternMatching where

import Chapter2.DataTypes

--ghci Chapter2.PatternMatching -Wall

clientName :: Client -> String 
clientName client = case client of 
                      GovOrg name                  -> name 
                      Company name _ _ _           -> name
                      Individual (Person fNm lNm _) _ -> fNm ++ " " ++ lNm

myclient :: Client
myclient = Company "Name" 2 (Person "nathan" "Swindall" Male) "Boss"

myclient2 :: Client
myclient2 = GovOrg "NASA"


companyName :: Client -> Maybe String 
companyName client = case client of 
                        Company name _ _ _ -> Just name 
                        _                  -> Nothing

myName :: String
Just myName = companyName myclient




--Two examples of bad patterMatching
f :: Client -> String 
f client = case client of 
             Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
             _                                    -> "There is no boss"
bossClient :: Client
bossClient = Company "Nasa" 1 (Person "John" "Smith" Male) "Boss"

notBoss :: Client 
notBoss = GovOrg "Nasa"

notBoss2 :: Client 
notBoss2 = Company "Nasa" 2 (Person "John" "Smith" Male) "Director"


--This is a bad example
gx :: Client -> String 
gx client = case client of 
             Company _ _ (Person name _ _) pos -> 
                case pos of "Boss" -> name ++ " is the boss"
             _                                 -> "There is no boss"



--Instead of pattern matching you can also do just regular names
clientName_ :: Client -> String 
clientName_ (GovOrg name)                     = name
clientName_ (Company name _ _ _)              = name
clientName_ (Individual (Person fNm lNm _) _) = fNm ++ " " ++ lNm

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


--Exercise 2-5
{-
write a function that returns the number of clients of each gender. You may need to define an auxilliary data type to hold the results of the function
-}


--Test Client List
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

numClientsMale :: [Client] -> Int
numClientsMale [] = 0
numClientsMale (c:cs) = (numClientHelper Male c) + numClientsMale cs


numClientHelper :: Gender -> Client -> Int
numClientHelper _ (GovOrg _) = 0
numClientHelper gen (Company _ _ (Person _ _ g) _) = rightGen gen g
numClientHelper gen (Individual (Person _ _ g) _) = rightGen gen g

rightGen :: Gender -> Gender -> Int
rightGen gen g = if gen == g then 1 else 0


{-
write a function that given a list of time machines, decreases their price by some percentage
Use data type you defined in Exercise 2 -4
-}



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

discountTimeMachines :: Float -> [TimeMachine] -> [TimeMachine]
discountTimeMachines dis = map (discountPriceTM dis) 

discountPriceTM :: Float -> TimeMachine -> TimeMachine
discountPriceTM dis (TimeMachine m i n pf price) = TimeMachine m i n pf (price*(1 - dis))

discountTimeMachine1 :: TimeMachine
discountTimeMachine1 = discountPriceTM (0.20) timeMachine1