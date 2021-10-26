module Chapter2.DataTypes where


-- -
-- data Client = GovOrg String
--              | Company String Integer String
--              | Individual String String Bool
--              deriving Show

data Client = GovOrg String
            | Company String Integer Person String --record name, identification number, contact person, person's poistion
            | Individual Person Bool -- Person's name and whether they want to recieve more information 
            deriving Show 

-- data Person = Person String String 
--             deriving Show

data Gender = Male | Female | Unknown 
            deriving (Show,Eq)

data Person = Person String String Gender
            deriving Show


--Time Machine data type
--manufacturer, model num (integer), their name, can they travel to the past and to the future, price (float)
data TimeMachine = TimeMachine String Integer String TravelDirection Float
                        deriving Show

data TravelDirection = Past 
                      | Present 
                      | Future 
                      | PastFuture
                      deriving Show


