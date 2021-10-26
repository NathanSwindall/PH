{-# LANGUAGE RecordWildCards #-}
module Chapter4.Classes where 


--Data declarations
data Person = Person { firstName :: String, lastName :: String}
            deriving (Show, Ord)

data Client i = GovOrg { clientId :: i, clientName :: String}
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty:: String }
              | Individual { clientId :: i, person :: Person}
              deriving (Show)

data TimeMachine = TimeMachine  { manufacturer :: String
                                , modelNum :: Integer
                                , companyName :: String
                                , direction :: TravelDirection
                                , price :: Double}
                                deriving Show

data TravelDirection = Past
                     | Present 
                     | Future 
                     | PastFuture
                     deriving Show

--class declarations
class Nameable n where 
    name :: n -> String

class Priceable p where 
    getPrice :: p -> Double 



--Function declarations
totalPrice :: Priceable p => [p] -> Double 
totalPrice [] = 0
totalPrice (p:ps) = getPrice p + totalPrice ps


initial :: Nameable n => n -> Char 
initial n = head (name n)


testInd :: Client Integer 
testInd = Individual {person=Person {firstName="Nathan", lastName="Swindall"},clientId=1}

testGov :: Client Integer
testGov = GovOrg {clientId=123, clientName= "Nasa"}

testCompany :: Client Integer
testCompany = Company {clientId=1
                      ,clientName="Nasa"
                      , person=Person{firstName="a", lastName="b"}
                      , duty="director"}

testTM :: TimeMachine
testTM = TimeMachine { manufacturer="People's Republic"
                     , modelNum = 123 
                     , companyName = "China"
                     , direction = Future
                     , price = 22102.29
                     }

--Instance Declarations
-- instance Nameable (Client i) where 
--     name Individual { person = Person {firstName = f, lastName= n}}
--            = f ++ " " ++ n 
--     name c = clientName c

--you can use the RecordWildCards extension here
instance Nameable (Client i) where 
    name Individual {person = Person {..}} = firstName ++ " " ++ lastName
    name c = clientName c

instance Priceable (TimeMachine) where 
    getPrice TimeMachine {..} = price

instance Eq a => Eq (Client a) where 
    Individual {clientId=c1, person=p1} == Individual {clientId=c2,person=p2} = 
        c1 == c2 && p1 == p2 
    Company {clientId=c1, clientName=cn1, person=p1,duty=d1} == 
        Company {clientId=c2, clientName=cn2, person=p2,duty=d2} =
        c1 == c2 && cn1 == cn2 && p1 == p2 && d1 == d2 
    GovOrg {clientId=c1,clientName=cn1} == GovOrg {clientId=c2, clientName=cn2}=
        c1==c2 && cn1 ==cn2
    _  == _  = False


instance Eq Person where 
    Person {firstName=fn, lastName=ln}==Person {firstName=fn1, lastName=ln1} = 
        fn == fn1 && ln == ln1


getClientName :: Client i -> String 
getClientName cl = case cl of 
                   Individual {person=Person {..}} -> firstName ++ " " ++ lastName
                   _ -> clientName cl   

compareType :: Client i -> Client i -> Ordering
compareType (Individual _ _) (Individual _ _) = EQ 
compareType (Individual _ _) _ = GT 
compareType _ (Individual _ _) = LT
compareType (Company _ _ _ _) (GovOrg _ _) = GT
compareType (GovOrg _ _) (Company _ _ _ _) = LT
compareType _ _ = EQ


--compare client name
--if they coincide then Individual first, Company, and government
instance Eq a => Ord (Client a) where 
    compare c1 c2 = 
        let name1 = getClientName c1 
            name2 = getClientName c2 
        in 
            if name1 == name2 then
                compareType c1 c2  
            else if name1 <= name2 then LT 
            else GT
    -- compare Individual {person=p1,clientId=c1} Individual {person=p2,clientId=c2} = 
    --     if p1 == p2 then 
    --         if c1 == c2 then EQ 
    --         else if c1 <= c2 then LT 
    --         else GT
    --     else if p1 <= p2 then LT 
    --     else GT
    -- compare Individual {person=p1,clientId=c1} Company {person=p2}


--Completx numbers 
data Complex = C Double Double deriving (Show,Eq)

instance Num Complex where 
    (C a1 b1) + (C a2 b2) = C (a1 + a2) (b1 + b2 )
    (C a1 b1) - (C a2 b2) = C (a1 - a2) (b1 - b2)
    (C a1 b1) * (C a2 b2) = C (a1*a2-b1*b2) (a1*b2+b1*a2)
    negate (C a b)        = C (negate a) (negate b)
    fromInteger n         = C (fromInteger n) 0 
    abs (C a b)           = C (sqrt $ a*a + b * b) 0 
    signum c@(C a b)      = let C n _ = abs c in C (a/n) (b/n)