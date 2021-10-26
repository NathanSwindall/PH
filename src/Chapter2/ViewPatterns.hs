{-# LANGUAGE ViewPatterns #-}
module Chapter2.ViewPatterns where 
import Chapter2.DataTypes
import Chapter2.Test


--to Run:
--ghci Chapter2.ViewPatterns -Wall
clientName_ :: Client -> String 
clientName_ (GovOrg name)                     = name
clientName_ (Company name _ _ _)              = name
clientName_ (Individual (Person fNm lNm _) _) = fNm ++ " " ++ lNm


responsibility :: Client -> String 
responsibility (Company _ _ _ r) = r 
responsibility _                 = "Unknown"

a :: Client
a = company1

specialClient :: Client -> Bool 
specialClient (clientName_ -> "Mr. Swindall") = True 
specialClient (responsibility -> "Director")  = True 
specialClient _                               = True 

--Test case should be true
swindall=  Individual (Person "Mr." "Swindall" Male) True