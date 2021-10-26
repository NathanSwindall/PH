{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE  RecordWildCards #-}
module Chapter2.Records where 

import Data.Char

--ghci Chapter2.Records -Wall
data ClientR = GovOrgR { clientRName :: String}
              | CompanyR { clientRName :: String 
                         , companyId :: Integer 
                         , person :: PersonR 
                         , duty :: String }
              | IndividualR { person :: PersonR}
                  deriving Show 

data PersonR = PersonR { firstName :: String 
                       , lastName :: String 
                       } deriving Show



--test data
nasa :: ClientR
nasa = GovOrgR { clientRName = "Nasa"}

noRedInk :: ClientR
noRedInk = CompanyR { clientRName = "NoRedInk"
                    , companyId = 124
                    , person = PersonR { firstName = "John", lastName = "Smith" }
                    , duty = "Lead Programmer"}

nathan :: ClientR
nathan = IndividualR { person = PersonR {firstName= "Nathan", lastName="Swindall"}}


--You can use the record field as functions
nathanName :: String
nathanName = firstName (person nathan) ++ " " ++ lastName (person nathan)


--You don't need to include all the fields and you can pattern match on them
greet :: ClientR -> String 
greet IndividualR { person = PersonR { firstName = fn}} = "Hi, " ++ fn 
greet CompanyR    { clientRName = c} = "Hi, " ++ c 
greet GovOrgR     {}                 = "Welcome"


--We can add an extension to compiler to use field names without setting up variables
-- We add {-# LANUGAGE NamedFieldPuns #-}
greet2 :: ClientR -> String 
greet2 IndividualR { person = PersonR {firstName}} = "Hi, " ++ firstName 
greet2 CompanyR { clientRName} = "Hi, " ++ clientRName 
greet2 GovOrgR  {}             = "Welcome"


--We can go one step further by using  using recordWildCards
--this will make bindings for all the fields
-- Use compiler extension {-# LANGUAGE RecordWildCards #-}
greet3 :: ClientR -> String 
greet3 IndividualR { person = PersonR {..}} = "Hi, " ++ firstName 
greet3 CompanyR {..}                        = "Hi, " ++ clientRName 
greet3 GovOrgR  {}                          = "Welcome"



--Now Let's update a record
--This will capitalized the first name
nameInCapitals :: PersonR -> PersonR 
nameInCapitals p@(PersonR {firstName = initial:rest}) = 
    let newName = (toUpper initial): rest
    in p {firstName = newName}
nameInCapitals p@(PersonR {firstName = ""}) = p

testperson :: PersonR
testperson = PersonR {firstName = "nathan", lastName = "swindall"}


--Exercise 2-7 
data TimeMachine = TimeMachine  { manufacturer :: String
                                , modelNum :: Integer
                                , companyName :: String
                                , direction :: TravelDirection
                                , price :: Float}
                                deriving Show

data TravelDirection = Past
                     | Present 
                     | Future 
                     | PastFuture
                     deriving Show

--update prices with the new records
updateTimeMachinePrice :: Float -> TimeMachine -> TimeMachine 
updateTimeMachinePrice p tm@(TimeMachine {..}) = tm {price = p}

--you can actually update it this way too
updateTimeMachinePrice2 :: Float -> TimeMachine -> TimeMachine
updateTimeMachinePrice2 p tm = tm {price = p}

nathansTimeMachine :: TimeMachine
nathansTimeMachine = TimeMachine
                     { manufacturer = "Swindeasy LLC"
                     , modelNum = 1
                     , companyName= "Swindeasy LLC"
                     , direction= PastFuture
                     , price = 1000.01}