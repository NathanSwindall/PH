module Chapter4.BinaryTrees where 

data TravelGuide = TravelGuide { title :: String
                               , authors :: [String ]
                               , price :: Double }
                deriving (Show, Eq, Ord)

data BinaryTree1 = Node1 TravelGuide BinaryTree1 BinaryTree1
                 | Leaf1
                 deriving Show 

treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide 
treeFind1 t (Node1 v l r) = case compare t v of 
                              EQ -> Just v 
                              LT -> treeFind1 t l 
                              GT -> treeFind1 t r
treeFind1 _ (Leaf1)        = Nothing


insertBranch:: TravelGuide -> BinaryTree1 -> BinaryTree1 
insertBranch t (Node1 v l r) = case compare t v of 
                                EQ -> (Node1 v l r)
                                LT -> (Node1 v (insertBranch t l) r)
                                GT -> (Node1 v l (insertBranch t r))
insertBranch t Leaf1           = Node1 t Leaf1 Leaf1


tg1 :: TravelGuide 
tg1 = TravelGuide "National Geographi" ["Nathan Thomas"] 45.45

tg2 :: TravelGuide 
tg2 = TravelGuide "Luxury Travles" ["Sam Atkins"] 47.69

tg3 :: TravelGuide 
tg3 = TravelGuide "Ireland CountrySide Travels" ["Mac O'Reily"] 29.32

tg4 :: TravelGuide 
tg4 = TravelGuide "Sea Side Adventures" ["Judus Priest"] 23.12

tg5 :: TravelGuide 
tg5 = TravelGuide "Budgeter's Guide to the World" ["Angel Zamora"] 38.43


tg6 :: TravelGuide
tg6 = TravelGuide "Taking a trip in 90 days" ["Pirate Steve"] 34.32

tg7 :: TravelGuide
tg7 = TravelGuide "Cheap Travels" ["Sleezy Rick"] 1.01

-- Node1 (TravelGuide {title = "National Geographi", authors = ["Nathan Thomas"], price = 45.45}) 
--       L(Node1 (TravelGuide {title = "Luxury Travles", authors = ["Sam Atkins"], price = 47.69}) 
--         LL(Node1 (TravelGuide {title = "Ireland CountrySide Travels", authors = ["Mac O'Reily"], price = 29.32}) 
--          LL(Node1 (TravelGuide {title = "Budgeter's Guide to the World", authors = ["Angel Zamora"], price = 38.43}) Leaf1 Leaf1) Leaf1) Leaf1) 
--         R(Node1 (TravelGuide {title = "Sea Side Adventures", authors = ["Judus Priest"], price = 23.12}) Leaf1 
--        RR (Node1 (TravelGuide {title = "Taking a trip in 90 days", authors = ["Pirate Steve"], price = 34.32}) Leaf1 Leaf1))


--Node1 (tg1 
--        Left : (Node1 tg2   
--                   Left:  (Node1 tg3 
--                           Left: (Node1 tg5 Leaf1 Leaf1) 
--                           Right: Leaf1) 
--                   Right:  Leaf1  ) 
--        Right: (Node1 tg4  --Left
--                   Left:  (Leaf1) 
--                   Right: (Node tg6 Leaf1 Leaf1))

--Polymorphic Binary Trees
data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                   | Leaf2 
                   deriving Show 

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a 
treeFind2 t (Node2 v l r) = case compare t v of 
                                EQ -> Just v 
                                LT -> treeFind2 t l
                                GT -> treeFind2 t r
treeFind2 _ Leaf2          = Nothing

treeInsert :: Ord a => BinaryTree2 a -> a -> BinaryTree2 a
treeInsert n@(Node2 v l r) tg = case compare v tg of 
                                    EQ -> n 
                                    LT -> Node2 v (treeInsert l tg) r
                                    GT -> Node2 v l (treeInsert r tg)
treeInsert Leaf2 tg           = Node2 tg Leaf2 Leaf2

startTree :: BinaryTree2 a
startTree = Leaf2

makeTree :: Ord a => [a] -> BinaryTree2 a -> BinaryTree2 a
makeTree [] bt     = bt
makeTree (a:as) bt = makeTree as (treeInsert bt a) 

travelGuides :: [TravelGuide]
travelGuides = [tg1,tg2,tg3,tg4,tg5,tg6, tg7]


--Make the tree
madeTree :: BinaryTree2 TravelGuide
madeTree = makeTree travelGuides startTree


--Lets order by price now
data TravelGuide2 = TravelGuide2 { title2 :: String
                                 , authors2 :: [String ]
                                 , price2 :: Double }
                    deriving (Show, Eq)

instance Ord TravelGuide2 where 
    (TravelGuide2 t1 a1 p1) <= (TravelGuide2 t2 a2 p2) = 
        p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

tg1_2 :: TravelGuide2
tg1_2 = TravelGuide2 {title2 = "Adventures In ProgramLando"
                     , authors2 = ["Nathan", "Thomas"]
                     , price2 = 99.9
                     }

tg2_2 :: TravelGuide2
tg2_2 = TravelGuide2 {title2 = "Zdventures In ProgramLando"
                     , authors2 = ["Nathan", "Thomas"]
                     , price2 = 99.8
                     }                 


newtype TGByPrice = TGByPrice TravelGuide
                        deriving Eq


--If we want to order differently by Price we can make a new type to do this
--Now we can create functions that order them differently. 
instance Ord TGByPrice where 
    (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) = 
        p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c) 
                     | Leaf3 
                     deriving (Show, Eq, Ord)

treeInsert3 :: (Ord v, Ord c)
            => v -> c -> BinaryTree3 v c -> BinaryTree3 v c 
treeInsert3 v c (Node3 v2 c2 l r)
            = case compare v v2 of 
                EQ -> Node3 v2 c2 l r 
                LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r 
                GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)
treeInsert3 v c Leaf3 = Node3 v c Leaf3 Leaf3 

--create a sum of all prices tree
--caching the price
--There are a few different ways you could have the price
--You could cache the travel guide with the smallest price 
--Or you could cache just the price

--1st example means you will effectively have treeInsert v v 
-- Second example mean you will effectively have treeInsert v p

--StartTree 
--List of different TravelGuides 
--Make Tree with insert (using price function) (make an infinity type)

-- data Infinity a = PInfinity 
--                 | Num a 
--                 | NInfinity
--                 deriving (Show, Eq)

-- instance Ord a => Ord (Infinity a) where 
--     PInfinity <= Num _ = False
--     PInfinity <= NInfinity = False 
--     NInfinity <= Num _ = True
--     PInfinity <= PInfinity = True 
--     NInfinity <= PInfinity = True 
--     NInfinity <= NInfinity = True 
--     (Num _)   <= PInfinity = True 
--     (Num _) <= NInfinity  = False
--     (Num n1) <= (Num n2)   = n1 <= n2

--you could default to this so you don't have to write the instance for ord
data Infinity a = NInfinity 
                | Num a 
                | PInfinity 
                deriving (Show, Eq, Ord)

startBinaryTree3 :: BinaryTree3 (TravelGuide) (Double)
startBinaryTree3 = Leaf3


--travelGuides
makeBinaryTree3 ::BinaryTree3 TravelGuide Double -> [TravelGuide] -> BinaryTree3 TravelGuide Double
makeBinaryTree3 st [] = st
makeBinaryTree3 st@(Leaf3) (tg:tgs) = makeBinaryTree3 (treeInsert3 tg (price tg) st) tgs
makeBinaryTree3 st (tg:tgs) =  makeBinaryTree3 (treeInsert3 tg (price tg) st) tgs

cheap :: BinaryTree3 TravelGuide Double
cheap = makeBinaryTree3 startBinaryTree3 travelGuides

startTreeMin :: BinaryTree3 TravelGuide Min 
startTreeMin = Leaf3

makeBinaryTreeMin ::BinaryTree3 TravelGuide Min -> [TravelGuide] -> BinaryTree3 TravelGuide Min
makeBinaryTreeMin st [] = st
makeBinaryTreeMin st@(Leaf3) (tg:tgs) = makeBinaryTreeMin (treeInsert4 tg (Min (price tg)) st) tgs
makeBinaryTreeMin st (tg:tgs) =  makeBinaryTreeMin (treeInsert4 tg (Min (price tg)) st) tgs


mintree = makeBinaryTreeMin startTreeMin travelGuides

--with monoids and semigroups
treeInsert4 :: (Ord v, Monoid c)
            => v -> c -> BinaryTree3 v c -> BinaryTree3 v c 
treeInsert4 v c (Node3 v2 c2 l r)
  = case compare v v2 of 
      EQ -> Node3 v2 c2 l r 
      LT -> let newLeft = treeInsert4 v c l 
                newCache = c2 <> cached newLeft <> cached r 
            in Node3 v2 newCache newLeft r 
      GT -> let newRight = treeInsert4 v c r 
                newCache = c2 <> cached l <> cached newRight 
            in Node3 v2 newCache l newRight 
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3 

cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _ ) = c 
cached Leaf3            = mempty


instance Semigroup Double where 
    (<>) a b = a + b

testSemi :: Double
testSemi = (2.0::Double) <> (3.0::Double)

instance Monoid Double where 
    mempty = 0 
    mappend a b = a + b 
    mconcat [] = mempty
    mconcat (a:as) = a + mconcat as

newtype Min = Min Double deriving Show 

instance Semigroup Min where 
    Min x <> Min y = Min $ min x y 

instance Monoid Min where 
    mempty = Min infinity where infinity = 1/0
    mappend = (<>) --use definition from Semigroup

--Monoids for making weapons