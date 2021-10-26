{-#LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleContexts #-}
module Chapter6.Test where 
import qualified Data.Map as M
import Data.List -- gives you minimumBy

class Ord v => Vector v where 
    distance :: v -> v -> Double 
    centroid :: [v] -> v  

class Vector v => Vectorizable e v where 
    toVector :: e -> v



instance Vector (Double, Double) where 
    distance (a,b) (c,d) = sqrt $ (c-a) * (c-a) + (d-b)*(d-b)
    centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0,0) lst 
                       n = fromIntegral $ length lst 
                    in (u/n, v/n)   --average point location 



instance Vectorizable (Double,Double) (Double,Double) where 
    toVector = id



-- clusterAssignmentPhase :: (Ord v,Vector v, Vectorizable e v)
--                        => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points = 
                    let initialMap = initializeMap centroids
                    in sortPointsToCentroids points initialMap centroids


initializeMap :: (Vectorizable e v) => [v] ->  M.Map v [e]
initializeMap centroids = M.fromList $ zip centroids (repeat [])


sortPointsToCentroids :: (Vectorizable e v) => [e] -> M.Map v [e] -> [v]  -> M.Map v [e]
sortPointsToCentroids points initialMap cen = foldr (sortPoint cen) initialMap points --m = mapnts


sortPoint :: (Vectorizable e v) => [v] -> e -> M.Map v [e] -> M.Map v [e]
sortPoint cen point m = M.adjust (point:) (findNearestCentroid point cen) m
-- putPointsWithCorrectCentroid initialMap points = foldr (\p m -> M.adjust (p:) (getChosenCentroid p) m) initialMap points

findNearestCentroid :: (Vectorizable e v) => e -> [v] -> v
findNearestCentroid p cen = minimumBy (compareDistance p) cen --takes centroids

compareDistance :: (Vectorizable e v) => e -> v -> v -> Ordering
compareDistance p x y = compare (distance x $ toVector p)
                                (distance y $ toVector p)