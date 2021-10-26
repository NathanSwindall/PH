
module Chapter6.Monads where 
import qualified Data.Map as Map


type UserName = String 
type GamerId = Int 
type PlayerCredits = Int


--Functor (a -> b) -> m a -> m b 
-- <$> same as fmap
square x = x * x 
ex_example = fmap square (Just 5)

--applicatives
-- f (a -> b) -> f a -> f b 
-- pure puts it in context
ex_applicative = pure (++) <*> Just "Nathan" <*> Just " Swindall"

main :: IO () 
main = do 
    putStrLn "Remember  do-notation"
    putStrLn "It makes things easy!"

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1, "DragonSlayer1")
                          ,(2, "KinIgOFtheBeats")
                          ,(3, "DoinFUNSTuf234")
                          ,(4, "GONgdRDJF")
                          ,(5, "XCTHULHUX123")
                          ,(6, "NSWIN20202")]
creditsDB :: Map.Map UserName PlayerCredits 
creditsDB = Map.fromList [("DragonSlayer1",2000)
                         ,("KinIgOFtheBeats",3000)
                         ,("DoinFUNSTuf234",3000)
                         ,("GONgdRDJF",2000)
                         ,("XCTHULHUX123",3343)
                         ,("NSWIN20202",3434)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB 

lookupCredits :: UserName -> Maybe PlayerCredits 
lookupCredits username = Map.lookup username creditsDB 


--wrapper function 
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits 
altLookupCredits Nothing = Nothing 
altLookupCredits (Just username) = lookupCredits username


creditsFromId :: GamerId -> Maybe PlayerCredits 
creditsFromId id = altLookupCredits (lookupUserName id)

creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits)
creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id 
-- pure (Maybe (a -> Maybe c))
-- pure (maybe (a -> b)) -> m a -> m (Maybe c)  --this is bad

--create and echo function


-- creating a function
creditsFromId2 :: GamerId -> Maybe PlayerCredits 
creditsFromId2 id = lookupUserName id >>= lookupCredits


type WillCold = Int 

gamerIdDB :: Map.Map WillCold GamerId 
gamerIdDB = Map.fromList [(1001,1)
                         ,(1002,2)
                         ,(1003,3)
                         ,(1004,4)
                         ,(1005,5)
                         ,(1006,6)]

lookupGamerId :: WillCold -> Maybe GamerId 
lookupGamerId id = Map.lookup  id gamerIdDB

creditsFromWCId :: WillCold -> Maybe PlayerCredits 
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

doCreditsFromWCId id = do 
                gamerID <- lookupGamerId id 
                userName <- lookupUserName gamerID 
                credits <- lookupCredits userName
                return credits --puts back in context

echoVerbose :: IO () 
echoVerbose = putStrLn "Enter a String an we'll echo it!" >> 
              getLine >>= putStrLn

--throw away result

--build Hello name program
askForName :: IO () 
askForName = putStrLn "What is your name?"

nameStatement :: String -> String 
nameStatement name = "Hello, " ++ name ++ "!"

helloName = askForName >>
            getLine >>=
            (\name -> return (nameStatement name)) >>= 
            putStrLn

helloNameDo = do 
          askForName
          name <- getLine 
          let statement = nameStatement name 
          putStrLn statement

--modeling candidates 
data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate 
    { candidateId :: Int 
    , codeReview :: Grade 
    , cultureFit :: Grade 
    , education :: Degree} deriving Show 

viable :: Candidate -> Bool 
viable candidate = all (== True) tests 
            where passedCoding = codeReview candidate > B 
                  passedCultureFit = cultureFit candidate > C 
                  educationMin = education candidate >= MS 
                  tests = [passedCoding
                          ,passedCultureFit 
                          , educationMin]

testCandidate_T :: Candidate
testCandidate_T = Candidate 2 A A MS  

testCandidate_F :: Candidate 
testCandidate_F = Candidate 2 B C HS

readInt :: IO Int 
readInt = getLine >>= (return .read)

readGrade :: IO Grade 
readGrade = getLine >>= (return . read)

readDegree :: IO Degree 
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate 
readCandidate = do 
    putStrLn "enter id: "
    cId <- readInt 
    putStrLn "enter code grade: "
    codeGrade <- readGrade 
    putStrLn "enter culture fit grade: "
    cultureGrade <- readGrade 
    putStrLn "enter Education: "
    degree <- readDegree
    return (Candidate { candidateId = cId
                      , codeReview = codeGrade 
                      , cultureFit = cultureGrade
                      , education = degree})

viableCandidate :: IO Bool
viableCandidate = do 
                candidate <- readCandidate 
                return (viable candidate)

candidate1 :: Candidate 
candidate1 = Candidate { candidateId = 1
                       , codeReview = A 
                       , cultureFit = A
                       , education = BA}

candidate2 :: Candidate 
candidate2 = Candidate { candidateId = 2 
                       , codeReview = C 
                       , cultureFit = A 
                       , education = PhD}

candidate3 :: Candidate 
candidate3 = Candidate { candidateId = 3 
                       , codeReview = A 
                       , cultureFit = B 
                       , education = MS }

candidateDB :: Map.Map Int Candidate 
candidateDB = Map.fromList [(1, candidate1)
                           ,(2, candidate2)
                           ,(3, candidate3)]

assessCandidateMaybe :: Int -> Maybe String 
assessCandidateMaybe cId = do 
                candidate <- Map.lookup cId candidateDB 
                let passed = viable candidate 
                let statement = if passed 
                                then "Passed"
                                else "failed"
                return statement

candidates :: [Candidate]
candidates = [candidate1
             ,candidate2
             ,candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do 
            candidate <- candidates 
            let passed = viable candidate 
            let statement = if passed 
                            then "passed"
                            else "failed"
            return statement

assessCandidates :: [Candidate] -> [String]
assessCandidates candidates = map (\x -> if x 
                                        then "passed"
                                        else "failed") passed 
            where passed = map viable candidates

assessCandidate :: Monad m => m Candidate -> m String 
assessCandidate candidates = do 
            candidate <- candidates 
            let passed = viable candidate 
            let statement = if passed 
                            then "passed"
                            else "failed"
            return statement 

newtype PokemonBattle a = PokemonBattle a deriving (Show
                                                , Eq
                                                , Ord)

instance Functor (PokemonBattle) where 
    fmap f (PokemonBattle a) = PokemonBattle (f a)

instance Applicative (PokemonBattle) where 
    pure a = PokemonBattle a 
    (PokemonBattle a) <*> something = fmap a something 

instance Monad (PokemonBattle) where 
    return x = PokemonBattle x -- just like pure 
    PokemonBattle x >>= f = f x 
    --fail _ = error "Something went wrong"

candidatePokemonBattle :: PokemonBattle Candidate 
candidatePokemonBattle = PokemonBattle (Candidate 2343 A A PhD)

--pokemonBattle :: PokemonBattle String
pokemonBattle = do 
        passed <- assessCandidate candidatePokemonBattle
        return passed
           

pokemonBattle2 pb = do 
        passed <- assessCandidate pb
        return passed 

youJustNeedFunction = assessCandidate candidatePokemonBattle
