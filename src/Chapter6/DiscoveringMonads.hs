module Chapter6.DiscoveringMonads where 

import Data.Maybe
import qualified Data.Map as Map

-- meanPurchase :: Integer -- the client identifier
--              -> Double -- the mean purchase 
-- meanPurchase clientId = let p = purchasesByClientId clientId
--                         in foldr (+) 0.0 $ catMaybes $ map purchaseValue p 

-- purchaseValue :: Integer -> Maybe Double 
-- purchaseValue purchaseId = 
--     case numberItemsByPurchaseId purchaseId of 
--         Nothing -> Nothing 
--         Just n -> case productIdByPurchaseId purchaseId of 
--                   Nothing -> Nothing 
--                   Just prId -> case pricebyProductId prId of 
--                                Nothing    -> Nothing 
--                                Just price -> Just $(fromInteger n) * price 

-- purchasesByClientId _ = [1,2,3]
-- numberItemsByPurchaseId _ = 3
-- productIdByPurchaseId _ = 2
-- pricebyProductId _ = 1

thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b 
thenDo Nothing _ = Nothing 
thenDo (Just x) f = f x

purchaseValue :: Integer -> Maybe Double 
purchaseValue purchaseId = 
    numberItemsByPurchaseId purchaseId `thenDo` (\n ->   --100
    productIdByPurchaseId purchaseId   `thenDo` (\productId ->  --10  
    priceByProductId productId         `thenDo` (\price -> --1003 
    Just $ fromInteger n * price))) -- 100 * 1003


emptyMap = Map.empty

numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId id = Map.lookup id (Map.fromList [(1,100),(2,200),(3,300)])

productIdByPurchaseId :: Integer -> Maybe Integer
productIdByPurchaseId id = Map.lookup id (Map.fromList [(1,10),(2,20),(3,40)])

priceByProductId :: Integer -> Maybe Double
priceByProductId id = Map.lookup id (Map.fromList [(10,1003),(20,2020),(40,34303)])


fmapFunc f x = x `thenDo` (\y -> Just $ f y)
--thenDo (Just x) f = f x
--thendo x (\y -> Just $ f y)
--thenDo (Just x) 