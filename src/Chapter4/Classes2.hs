{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Chapter4.Classes2 where 

class Evolution a b where 
    evolve :: a -> b


instance Evolution Double Double where 
    evolve = id


class Evolution2 a b where 
    evolve2 :: a -> a
    stats :: b -> b

-- instance Evolution2 Double Double where
--     evolve2 = id

data Evol = Evol | Devol deriving Show


instance Evolution2 Double Evol where 
    evolve2 = id
    stats (Evol) = Devol
    stats (Devol) = Evol

-- myFunc :: Evolution2 a b => a -> a 
-- myFunc a = evolve2 a

newtype Stats = Stats { speed :: Float}
newtype Dragon = Dragon Stats

--I can't seem to do this at all for the multparameter version
--Evolution will need an instance, can you make generic instances?
--We can make a polymorphistic function 

class GetStats a b where 
    getStats :: a -> b

instance GetStats a Stats 