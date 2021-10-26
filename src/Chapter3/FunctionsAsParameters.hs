{-# LANGUAGE LambdaCase #-}
module Chapter3.FunctionsAsParameters where 

equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples t = map (\(x,y) -> x == y) t




--you unfortunately can't match on anonymous functions
sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of 
                                "Alejandro" -> "Hello, writer"
                                _           -> "Welcome, " ++ name
                     ) names 

square :: Float -> Float
square x = x*x

--But if you enable the lambdaCase. You can enable it
-- sayHello2 names = map (\case "Alejandro" -> "Hello, writer"
--                               name       -> "Welcome, " ++ name
--                       ) names
sayHello2 :: [String] -> [String]
sayHello2 names = map (\case "Alejandro" -> "Hello, writer"
                             name        -> "Welcome, " ++ name
                      ) names
