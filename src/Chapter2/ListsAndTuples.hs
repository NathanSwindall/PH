module Chapter2.ListsAndTuples where 


(+++) :: [a] -> [a] -> [a]
[] +++ list2 = list2 
(x:xs) +++ list2 = x:(xs +++ list2)

sorted :: [Integer] -> Bool
sorted []             =True 
sorted [_]            =True 
sorted (x:y:zs) = x < y && sorted (y:zs)


sorted_ :: [Integer] -> Bool
sorted_ []         = True 
sorted_ [_]        = True 
sorted_ (x : r@(y:_)) =  x < y && sorted r 


--We can do better than this
maxmin :: (Ord a, Num a) =>  [a] -> (a,a)
maxmin [] = (0,0)
maxmin [x] = (x,x)
maxmin (x:xs) = (if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min
                ) where (xs_max, xs_min) = maxmin xs




ifibonacci :: Integer -> Maybe Integer 
ifibonacci n = if n < 0 
               then Nothing 
               else case n of 
                   0 -> Just 0 
                   1 -> Just 1 
                   n' -> let Just f1 = ifibonacci (n' -1)
                             Just f2 = ifibonacci (n' -2)
                         in Just (f1 + f2)



--Using Guards
ifibonacci_ :: Integer -> Maybe Integer
ifibonacci_ n | n < 0     = Nothing 
ifibonacci_ 0             = Just 0 
ifibonacci_ 1             = Just 1 
ifibonacci_ n | otherwise = let Just f1 = ifibonacci_ (n-1)
                                Just f2 = ifibonacci_ (n-2)
                            in Just (f1 + f2) 
binom :: Integer -> Integer -> Integer
binom _ 0          = 1 
binom x y | x == y = 1   
binom n k          = (binom (n-1) (k-1)) + (binom (n-1) k)


--You can also use a function in guards and guards have back tracking 
--How can you test out back tracking 

multipleOf :: Integer -> Integer -> Bool 
multipleOf x y = (mod x y) == 0 


--How can I make this a better function?
--One that shows it is also a multiple of 2 and 5 for 10 
specialMultiples :: Integer -> String 
specialMultiples n | multipleOf n 2 = show n ++ " is multiple of 2"
specialMultiples n | multipleOf n 3 = show n ++ " is multiple of 3"
specialMultiples n | multipleOf n 5 = show n ++ " is multiple of 5"
specialMultiples n | otherwise      = show n ++ " is a beautiful number"


--Exercise 2-6 More Matches and Guards

ackermann :: Integer -> Integer -> Integer
ackermann n 0 = n + 1
ackermann 0 m | m > 0 = ackermann (m-1) 1
ackermann n m | m > 0 && n > 0 = ackermann (m-1) (ackermann m (n-1))





