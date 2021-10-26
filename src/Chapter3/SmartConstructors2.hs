{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Chapter3.SmartConstructors2 where 

import Chapter3.SmartConstructors

--Why would we want to pattern match like this without the constructor
-- prettyRange :: Range -> String 
-- prettyRange rng = case rng of 
--                     (r -> R a b) -> "[" ++ show a ++ "," ++ show b ++ "]"

-- examplePrettyRange:: String
-- examplePrettyRange =prettyRange (range 1 100)

-- pattern R :: Integer -> Integer -> Range 
-- pattern R a b <- Range a b 
--     where R a b = range a b






