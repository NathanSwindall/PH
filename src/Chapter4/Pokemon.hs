module Chapter4.Pokemon where 

data Stonetype = FireStone 
               | WaterStone 
               | ThunderStone
               | LeafStone 
               | IceStone 
               deriving Show


data Charmander = Charmander
                | Charmeleon 
                | Charizard 
                deriving Show

data Pickichu = Pickichu 
              | Raichu
              deriving Show

data NathansPokemon = BabyNathieon
                    | KidNathieon
                    | TeenagerNathieon
                    | AdultNathieon
                    deriving Show

class Evolution a where 
    evolve :: a -> a

class StoneEvolution a where 
    stoneEvolve:: Stonetype -> a -> a

instance Evolution Charmander where 
    evolve Charmander = Charmeleon 
    evolve Charmeleon = Charizard 
    evolve Charizard = Charizard

instance Evolution Pickichu where 
    evolve Pickichu = Raichu
    evolve Raichu = Raichu

instance Evolution NathansPokemon where 
    evolve a = a

instance StoneEvolution NathansPokemon where 
    stoneEvolve FireStone BabyNathieon = KidNathieon 
    stoneEvolve WaterStone KidNathieon = TeenagerNathieon 
    stoneEvolve IceStone TeenagerNathieon = AdultNathieon 
    stoneEvolve _ a = a

--a few ways to model pokemon

data Stats = Stats { abilities :: [String]
                    , height :: Float
                    , weight :: Float 
                    , species :: String
                    , type_ :: [String]
                    }
                    deriving (Show, Eq)

data Bulbasaur = Bulbasaur Stats
                | Ivysaur Stats 
                | Venusaur Stats
                deriving (Show, Eq)

myBulbasaur :: Bulbasaur
myBulbasaur = Bulbasaur (Stats ["Overgrow"] 0.7 6.9 "Seed Pokemon" ["Grass", "Monster"])
 

--I want a way to get Stats out of even type
getStats (Bulbasaur stats) = stats



