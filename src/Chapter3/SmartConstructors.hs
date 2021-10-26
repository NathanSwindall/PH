{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Chapter3.SmartConstructors (Range(), range, pattern R) where

--Range() only exports the data type and not the constructor

data Range = Range Integer Integer deriving Show 

range :: Integer -> Integer -> Range 
range a b = if a <= b then Range a b else error "a must b <=b"

--We can't patter match using case when we limit only the data type Range
--is exported

-- data RangeObs = R Integer Integer deriving Show

-- r :: Range -> RangeObs
-- r (Range a b) = R a 


pattern R :: Integer -> Integer -> Range 
pattern R a b <- Range a b 
    where R a b = range a b