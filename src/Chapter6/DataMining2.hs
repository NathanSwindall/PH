{-#LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Chapter6.DataMining2 where 

import Data.List 
import qualified Data.Map as M
--ghci Chapter6.DataMining2 -Wall


class Ord v => Vector v where 
    distance :: v -> v -> Double 
    centroid :: [v] -> v

class Vector v => Vectorizable e v where 
    toVector :: e -> v 


--This class if for transfering one type of data into a vector
--I am guessing that the toVector id is because our data is already
-- in a shape that is good to do it with
instance Vectorizable (Double,Double) (Double,Double) where 
    toVector = id

instance Vector (Double, Double) where 
    distance (a,b) (c,d) = sqrt $ (c-a) * (c-a) + (d-b)*(d-b)
    centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0,0) lst 
                       n = fromIntegral $ length lst 
                    in (u/n, v/n)   --average point location 


-- instance Vector (Double, Double) where 
--     distance (a,b) (c,d) = sqrt $ (c-a)^2 + (d-b)^2

-- instance Vector (Double, Double, Double) where 
--     distance (a,b,c) (d,e,f) = sqrt $ (d-a)*(d-a) + (e-b)*(e-b) + (f-c)*(f-c)

testCoordA :: (Double,Double)
testCoordA = (2.0,1.0)

testCoordB :: (Double,Double)
testCoordB = (10.0,12.0)

testC :: (Double,Double,Double)
testC = (2.0, 5.3, 20)

testD :: (Double, Double, Double)
testD = (2.0,3.0, 10.1)


--The power comes that you can use the same function just on different types
--whioch is really cool
--You get the same answer
--maybe it is because the power has to be an interger
-- distance2 :: (Double,Double) -> (Double,Double) -> Double 
-- distance2 (a,b) (c,d) = sqrt $ (c-a)^2 + (d-b)^2


-- kMeans :: (Vector v, Vectorizable e v)
--        => Int  -- number of centroids 
--        -> [e]  -- the information 
--        -> [v]  -- centroids after convergence 


--(Int -> [e] -> [v] -> [e]) is the random abstract function for initial centroids
-- kMeans :: (Vector v, Vectorizable e v)
--        => (Int -> [e] -> [v]) -> [e] -> [v]


--Forgy method is where we choose the starting centroids from the list already in place
--Map keys must have Ord, so our vector needs Ord



--Two phases
--   create the Map witha all the keys assigned to empty list 
--   go element by element and assign the 
---  must translate element into a vector first
clusterAssignmentPhase :: (Ord v,Vector v, Vectorizable e v)
                       => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points = 
    let initialMap = M.fromList $ zip centroids (repeat [])
      in foldr (\p m -> let chosenC = minimumBy (compareDistace p) centroids
                          in M.adjust (p:) chosenC m ) initialMap points 
    where compareDistace p x y = compare (distance x $ toVector p) 
                                         (distance y $ toVector p)




--M.fromList :: Ord k => [(k,a)] -> M.Map k a
--zip :: [a] -> [b] -> [(a,b)]




initializeMap centroids = M.fromList $ zip centroids (repeat [])


-- sortPointsToCentroids points initialMap = foldr (\point map -> M.adjust (p:) (findNearestCentroid point) map) initialMap points

findNearestCentroid p = minimumBy (compareDistance p) --takes centroids
compareDistance p x y = compare (distance x $ toVector p)
                                (distance y $ toVector p)


--clusterAssignmentPhase2 



centroidsM :: [(Double,Double)]
centroidsM  = [(2,3),(3,6),(10,1)]
pointsM = [(1,2),(3,4),(5,10), (10,2),(2,3),(1,2), (3,5),(4,6)]
initialMapM centroids = M.fromList $ zip centroids (repeat []) --[((1,2),[]),((2,3),[]),((3,4),[])]


getChosenCentroid p = minimumBy (compareDistance p) centroidsM




putPointsWithCorrectCentroid initialMap points = foldr (\p m -> M.adjust (p:) (getChosenCentroid p) m) initialMap points
--This is a weird function because it keeps adding stuff to the initialMap
-- m is inital Map

myInitialMap :: M.Map (Double,Double) [(Double,Double)] --without this you will get an error becasue of ambiguitiy
myInitialMap = initialMapM centroidsM
try = putPointsWithCorrectCentroid myInitialMap pointsM

testPoint :: (Double,Double)
testPoint = (3.0,4.0)
testPoint2 :: (Double,Double)
testPoint2 = (4.0,6.0)
testList = [1,2,3,4,5,6]
testLowest = minimumBy (compare) testList


-- M.adjust (p:) chosenC m ) initialMap it tkae sthe chosen centroid (one it's closest to) and then does the p: to add it to the list of points for that centroid
-- Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
-- M.adjust (p:) chosenC m (p:) it does p: on the list so it adds it to it. 
--foldr (-) 54 [10,11]
--foldr (-) (11 - 54) [10]
--foldr (-) (10 - (11 - 54 ))

--foldr (\a b -> a - b) 0 [10,11,12]
--10 - (11 - (12- 0))




zippedLists = zip [1,2,3,4,5] [1,2,3,4,5,6] --chops off the 6 one
fakeCentroids = [(1,2),(2,3),(3,4)]
zipFakeCentroids = zip fakeCentroids (repeat []) --[((1,2),[]),((2,3),[]),((3,4),[])]
mapCentroids = M.fromList zipFakeCentroids
-- lookedupValue = M.lookup mapCentroids (1,2)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid. map toVector)

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool 
shouldStop centroids threshold = 
    foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold 


kMeans :: (Vector v, Vectorizable e v)
       => (Int -> [e] -> [v]) -- initialization function 
       -> Int                 -- number of centroids
       -> [e]                 -- the information 
       -> Double              -- threshold  
       -> [v]                 -- final centroids
kMeans i k points = kMeans' (i k points) points

kMeans' :: (Vector v, Vectorizable e v)
        => [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold = 
    let assignments  = clusterAssignmentPhase centroids points 
        oldNewCentroids =newCentroidPhase assignments 
        newCentroids    = map snd oldNewCentroids 
    in if shouldStop oldNewCentroids threshold 
       then newCentroids 
       else kMeans' newCentroids points threshold 

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = [] 
initializeSimple n v = (fromIntegral n, fromIntegral n)
                     : initializeSimple (n-1) v 

info = [(1,1),(1,2),(4,4),(4,5)] :: [(Double,Double)]
solution = kMeans initializeSimple 2 info 0.001

       