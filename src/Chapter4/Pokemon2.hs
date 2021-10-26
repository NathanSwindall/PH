{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Chapter4.Pokemon2 where 

--need multi class
class Pokemon f a where 
    evolve ::  a -> a
    stats :: f a -> a

data Stats = Stats { abilities :: [String]
                    , height :: Float
                    , weight :: Float 
                    , species :: String
                    , type_ :: [String]
                    }
                    deriving (Show, Eq)

data Charmander a = Charmander Stats
                | Charmeleon Stats
                | Charizard Stats
                deriving (Show, Eq)

-- instance Pokemon (Charmander a) where 
--     evolve (Charmander stats) = Charmeleon stats
--     evolve (Charmeleon stats) = Charizard stats
--     evolve (Charizard stats) = Charizard stats

exampleChar = Charmander Stats { abilities = ["run", "bite"]
                               , height = 7.4
                               , weight = 40.2
                               , species = "Dragon"
                               , type_ = ["fire", "Monster"]
                               }

-- instance Pokemon (Charmander) (Stats) where 
--     evolve (Charmander stats) = (Charmander stats)
--     stats (Charmander ) = stats