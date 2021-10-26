module Chapter2.DefaultValues where 

data ConnType = TCP | UDP deriving Show
data UseProxy = NoProxy | Proxy String deriving Show
data TimeOut = NoTimeOut | TimeOut Integer deriving Show
data Connection = Internet ConnOptions String deriving Show

-- connect :: String -> ConnType -> Integer -> UseProxy
--         -> Bool -> Bool -> TimeOut -> Connection 
-- connect s s i p ch 

data ConnOptions = ConnOptions { connType      :: ConnType
                               , connSpeed     :: Integer
                               , connProxy     :: UseProxy
                               , connCaching   :: Bool 
                               , connKeepAlive :: Bool
                               , connTimeOut   :: TimeOut}
                               deriving Show

connect' :: String -> ConnOptions -> Connection
connect' url options = Internet options url

connDefault :: ConnOptions 
connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut

---if developer has made direct use of the constructor then this is a problem
-- He will have to change a lot of code. Instead we will use smart constructors