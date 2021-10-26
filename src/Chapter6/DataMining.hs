{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Chapter6.DataMining where 


-- using a class makes it more flexible if we want to add more dimensions
-- to our vectors
class Vector v where 
    distance :: v -> v -> Double

instance Vector (Double, Double) where 
    distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)


--https://connectionrequired.com/blog/2009/07/my-first-introduction-to-haskell-extensions-flexibleinstances
--FlexibleInstances
class Something a where 
    doSomething :: a -> Integer 

instance Something Integer where 
    doSomething x = 1

instance Something [a] where 
    doSomething x = 2
--This makes it so you can't differentiate between a list of strings 
-- and a list of numbers


--Using multiple parameters
-- class Multiply a b where 
--     multiply :: a -> b -> Int

-- instance Multiply Int Int where 
--     multiply a b = a * b

data Triple a b c = Triple a b c deriving (Show, Ord, Eq)
data Twople a b = Twople a b deriving (Show, Ord, Eq)

class SpecialTriple a where
    calc :: a -> Integer

--This instance needs FlexibleInstances
instance SpecialTriple (Triple Integer Integer Integer) where 
    calc (Triple a b c) = a + b + c

instance SpecialTriple (Triple [Integer] [Integer] [Integer]) where 
    calc (Triple (a:as) (b:bs) (c:cs)) = a + b + c 

testTriple :: Triple [Integer] [Integer] [Integer]
testTriple = Triple [1,2,3,4,5] [1,2,3,5,2] [2,3,4,5,6]



--nedd multipleParams for
class MultipleParam mul box where 
    mul:: a -> mul (box a)

-- instance MultipleParam (Triple Int Int Int) (Twople Int Int) where 
--     mul 
instance MultipleParam (Maybe) (Triple Int Int) where 
    mul a = Just (Triple (2::Int) (4::Int) a)

testMul = mul 3 :: Maybe (Triple Int Int Int)