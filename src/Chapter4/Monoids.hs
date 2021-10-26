{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Chapter4.Monoids where 

--class semigroup
--(<>) :: a -> a -> a
--class monoid
--mempty :: a
-- mappend :; a -> a -> a

-- need <>
--class monoid
--only need mempty and mappend

data Stonetype = FireStone 
               | WaterStone 
               | ThunderStone
               | LeafStone 
               | IceStone 
               deriving Show

data Evolution = Glaceon
               | Leafeon 
               | Jolteon
               | Vaporeon 
               | Flareon
               deriving Show 

--maybe make a newtype
--mempty = eevee

--
data Evolve = Stone Stonetype 
            | Evolved Evolution 
            | Eevee
            | NoEvolution 
            deriving Show

instance Semigroup Evolve where
    Stone FireStone <> Eevee = Evolved Flareon
    Stone WaterStone <> Eevee = Evolved Vaporeon
    Stone ThunderStone <> Eevee = Evolved Jolteon
    Stone LeafStone <> Eevee = Evolved Leafeon
    Stone IceStone <> Eevee = Evolved Glaceon
    Eevee <> Stone FireStone = Evolved Flareon
    Eevee <> Stone WaterStone = Evolved Vaporeon
    Eevee <> Stone ThunderStone = Evolved Jolteon
    Eevee <> Stone LeafStone = Evolved Leafeon
    Eevee <> Stone IceStone = Evolved Glaceon
    _ <> _ = NoEvolution

instance Monoid Evolve where 
    mempty = Eevee
    mappend = (<>)

eevees :: [Evolve]
eevees = repeat Eevee

fireStones :: [Evolve]
fireStones = repeat (Stone FireStone)

zippedEvolve :: [(Evolve,Evolve)]
zippedEvolve = zip (take 20 fireStones) eevees

makeFlareons :: [(Evolve, Evolve)] -> [Evolve]
makeFlareons ev = map (\(e,s) -> e <> s) ev



--Sylveon 
--Espeon 
--Umbreon

data Eve = Eve
 
class EeveeEve stone evee where 
    evolve :: stone -> evee -> Evolution

instance EeveeEve Stonetype Eve where
    evolve FireStone Eve = Flareon
    evolve LeafStone Eve = Leafeon
    evolve ThunderStone Eve = Jolteon
    evolve IceStone Eve     = Glaceon
    evolve WaterStone Eve   = Vaporeon























