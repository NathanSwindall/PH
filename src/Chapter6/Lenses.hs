{-# LANGUAGE LambdaCase #-} --Allows you to use \case
{-# LANGUAGE TemplateHaskell #-}
module Chapter6.Lenses where 

import Lens.Micro.Platform
import Data.Char
    

data Client i = GovOrg i String 
              | Company i String Person String 
              | Individual i Person 
              deriving (Show, Eq)

data Person = Person String String deriving (Show, Eq)

data PersonN i = PersonN i i

--examples of partial lenses
--with partial lens the type of the value doesn't change
-- firstName :: Lens' Person String 
-- firstName = lens (\(Person f _) -> f)
--                  (\(Person _ l) newF -> Person newF l)

-- lastName :: Lens' Person String 
-- lastName = lens (\(Person _ l) -> l)
--                 (\(Person f _) newL -> Person f newL)


-- identifier :: Lens (Client i) (Client j) i j
-- identifier = lens (\case (GovOrg i _)       -> i 
--                          (Company i _ _ _)  -> i 
--                          (Individual i _)   -> i) 
--                   (\client newId -> case client of 
--                       GovOrg _ n      -> GovOrg newId n 
--                       Company _ n p r -> Company newId n p r
--                       Individual _ p  -> Individual newId p)


--These properties get added to the Person properties
--They will not change the type
fullName :: Lens' Person2 String 
fullName = lens (\(Person2 f l) -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of 
                                        f:l:_ -> Person2 f l 
                                        _     -> error "Incorrect name")

crazyName :: Lens' Person2 String 
crazyName = lens (\(Person2 f l) -> crazyNameHelper f ++ " " ++ crazyNameHelper l)
                 (\_ newFullName -> case words newFullName of 
                                       f:l:_ -> Person2 f l 
                                       _     -> error "Incorrect name")

crazyNameHelper :: String -> String 
crazyNameHelper [] = ""
crazyNameHelper [a] = (toUpper a):[]
crazyNameHelper (a:b:as) = (toUpper a):(chr $ (ord b)+3):crazyNameHelper as
-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f tSource#

-- Lens s t a b is the lowest common denominator of a setter and a getter, something that has the power of both; it has a Functor constraint, and since both Const and Identity are functors, it can be used whenever a getter or a setter is needed.

-- a is the type of the value inside of structure
-- b is the type of the replaced value
-- s is the type of the whole structure
-- t is the type of the structure after replacing a in it with b

data MyNum1 a = MyNum1 a deriving Show
data MyNum2 b= MyNum2 b deriving Show

changePerson :: Show a =>  Lens (MyNum1 a) (MyNum2 String) a String
changePerson = lens (\(MyNum1 f) -> f)
                    (\(MyNum1 a) b-> MyNum2 (show a))

changeNumType :: Show a =>  Lens (MyNum1 a) (MyNum2 b) a b
changeNumType = lens (\(MyNum1 f) -> f)
                    (\_ b-> MyNum2 b)

changeNumTypeBack :: Lens (MyNum2 a) (MyNum1 b) a b
changeNumTypeBack = lens (\(MyNum2 f) -> f)
                         (\_ b -> MyNum1 b)


myNum = MyNum1 1000

example = set changePerson "Doesn't have a value" myNum
example2 = set changeNumType (MyNum1 100) myNum
example3 = set changeNumTypeBack (MyNum1 "nathan was here") example2

-- --client
-- client = Individual2 3 (Person2 "John" "Smith")


--Different operations -- you must have these after you make the lense
--ex_set = set identifier 4 client
--ex_lastName = view (person . lastName) client


--changePerson :: Lens (MyNum1) (MyNum2) (MyNum1) (MyNum2)
-- changePerson = lens (\(MyNum1 f) -> f)
--                     (\(MyNum1 a) b-> MyNum2 (show a))

--This is a lot of boiler plate, so let's change it up
--To work the fields must have an underscore in them
data Client2 i = GovOrg2 {_identifier :: i, _name :: String }
              | Company2 {_identifier :: i, _name :: String 
                        , _person :: Person2, _duty :: String}
              | Individual2 {_identifier :: i, _person :: Person2 }
              deriving Show 

data Person2 = Person2 {_firstName :: String, _lastName :: String }
              deriving Show

makeLenses ''Client2
makeLenses ''Person2

client = Individual2 3 (Person2 "John" "Smith")
            
ex_set_id = set identifier 4 client
ex_lastName = view (person . lastName) client -- "Smith"
ex_fullName = client ^. person . fullName
ex_crazyName = client ^. person. crazyName
ex_change_lastName = person . lastName .~ "Nathan" $ client 
ex_switched = client & person . fullName .~ "Nathan is the best" --set the new value
ex_update = client & identifier +~ 2

--over and %~
ex_update_different = client & over identifier (+100)
ex_update_different2 = client & identifier %~ (+1000)

ex_tuple_operations = ("a", "b") & set _1 "c"

-- person :: Applicative f => (Person2 -> f Person2) -> Client2 i -> f (Client2 i)
-- person . lastName :: Applicative f => (String -> f String) -> Client2 i -> f (Client2 i)
-- lastName  :: Functor f => (String -> f String) -> Person2 -> f Person2
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) lastName
--  :: Functor f => (a -> String -> f String) -> a -> Person2 -> f Person2
-- a = (String -> f String)
-- c = (Client2 i -> f (Client 2 i))
-- b = (Person2 -> f Person 2)
-- (.~) :: ASetter s t a b -> b -> s -> t
-- ( person . lastName .~) :: String -> Client2 i -> Client2 i
-- a is the type of the value inside of structure
-- b is the type of the replaced value
-- s is the type of the whole structure
-- t is the type of the structure after replacing a in it with b


--Using lenses
-- Time Machine example
--
data TimeMachine = TimeMachine  { _manufacturer :: String
                                , _modelNum :: Integer
                                , _companyName :: String
                                , _direction :: TravelDirection
                                , _price :: Double}
                                deriving Show

data TravelDirection = Past
                     | Present 
                     | Future 
                     | PastFuture
                     deriving Show

makeLenses ''TimeMachine

exampleTM1 = TimeMachine "Nasa" 123 "Nasa" Future 40
exampleTM2 = TimeMachine "US Government" 1234 "Veterans Affairs" PastFuture 41.23
exampleTM3 = TimeMachine "Google" 122 "Google TM section" Present 332.32

timeMachines = exampleTM1 : exampleTM2 : exampleTM3 : []


discountTMS :: [TimeMachine] -> Double -> [TimeMachine]
discountTMS [] dis = []
discountTMS (t:ts) discount = let dis = 1 - discount
                                  p   = _price t
                              in t { _price = p * dis} : (discountTMS ts discount)  

-- discountTMS2 :: [TimeMachine] -> Double -> [TimeMachine]
-- discountTMS2 [] dis = []
-- discountTMS2 (t:ts) discount = t {_price = p * (1-discount)} : (discountTMS2 ts discount)
                               
                                              
-- data Box = Box {field1:: String , field2 :: String } deriving Show
-- myBox = Box "Nathan" "Swindall"
--  myBox2 = myBox {field2 = "Thomas"}
-- Box {field1 = "Nathan", field2 = "Thomas"}

--Now let's discount with lenses
-- To change 
change45 tm = price .~ 45 $ tm

discountLenses :: [TimeMachine] -> Double -> [TimeMachine]
discountLenses [] discount = []
discountLenses (t:ts) discount = (price .~ ((*) (_price t) (1-discount)) $ t) : discountLenses ts discount


--view patterns maybe
discountLenses2 [] discount = [] 
discountLenses2 (t@(TimeMachine {_price=p}):ts) discount = (price .~ ((1-discount) * p) $ t) : discountLenses2 ts discount


data NotVirgen a = NotVirgen a deriving (Show)

hadSex a = NotVirgen a
hadSexWithJenny a = NotVirgen (a ++ " and Jenny")

instance Functor (NotVirgen) where 
    fmap f (NotVirgen a) = NotVirgen (f a)

instance Applicative (NotVirgen) where 
    pure a = NotVirgen a 
    (NotVirgen a) <*> something = fmap a something 

instance Monad (NotVirgen) where 
    return x = NotVirgen x -- just like pure 
    NotVirgen x >>= f = f x 
    --fail _ = error "Something went wrong"


--test out the monad
--(>>=) :: Monad m => m a -> (a -> m b) -> m b
myMonadFunc a = NotVirgen ("Thomas " ++ a)
notVirgen = NotVirgen "Nathan"

--visual novel elm



