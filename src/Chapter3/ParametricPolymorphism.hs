module Chapter3.ParametricPolymorphism where 

--ghci Chapter3.ParametricPolymorphism -Wall
--ghic Chapter3.ParametricPolymorphics -fno-warn-type-defaults

--ad hoc --when necessary

data Client i = GovOrg {clientId :: i, clientName :: String}
              | Company {clientId :: i, clientName :: String
                        , person :: Person, duty :: String}
              | Individual {clientId:: i, person :: Person}
              deriving (Show, Eq, Ord)

data Person = Person { firstName :: String, lastName :: String}
              deriving (Show, Eq, Ord)

--Can have different types
data Triple a b c = Triple a b c deriving (Show, Eq, Ord)

--Must have the same type
data TripleSame a = TripleSame a a a deriving (Show, Eq, Ord)

myTripleSame :: TripleSame Int 
myTripleSame = TripleSame 100 2 2

--Exercise 3-1
swapTriple :: (c, a, b) -> (a, b, c)
swapTriple (x,y,z) = (y,z,x)

duplicate :: a -> (a,a)
duplicate x = (x,x)

nothing :: p -> Maybe a
nothing _ = Nothing

index :: [a] -> [(Int,a)]
index []     = []
index [x]    = [(10,x)]
index (x:xs) = let indexed@((n,_):_) = index xs 
               in  (n+1,x):indexed
maybeA :: [a] -> Char
maybeA [] = 'a'


apply3f2 :: (Integer -> Integer) -> Integer -> Integer 
apply3f2 f x = 3 * f (x + 2)

--Special function
-- ($) :: (a -> b) -> a -> b 
-- f $ a = f a
--both sides of the operator will be evaluated first before ($) 
