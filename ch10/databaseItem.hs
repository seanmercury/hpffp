import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9002
  , DbDate (UTCTime
            (fromGregorian 1931 5 1)
            (secondsToDiffTime 34123))
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate di = getDbDate di []
  where 
    getDbDate (DbDate a:[]) curr = curr ++ [a]
    getDbDate (DbDate a:as) curr = getDbDate as (curr ++ [a])
    getDbDate (_:as) curr = getDbDate as curr

filterDbDate2 :: [DatabaseItem] -> [UTCTime]
filterDbDate2 = foldr getDbDate []
  where 
    getDbDate (DbDate a) b = a : b
    getDbDate _ b = b



-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber (DbNumber a : []) = [a]
filterDbNumber (_ : []) = []
filterDbNumber (DbNumber a : as) = filterDbNumber as ++ [a]
filterDbNumber (_ : as) = filterDbNumber as

filterDbNumber2 :: [DatabaseItem] -> [Integer]
filterDbNumber2 = foldr getDbNumber []
  where 
    getDbNumber (DbNumber a) b = a : b
    getDbNumber _ b = b



-- 3. 
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent di = getMax (filterDbDate2 di) (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))
  where 
    getMax (a : []) curmax = if a > curmax then a else curmax
    getMax (a : as) curmax = if a > curmax then getMax as a else getMax as curmax 

mostRecent2 :: [DatabaseItem] -> UTCTime
mostRecent2 = foldr getMax (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))
  where 
    getMax (DbDate a) as = if a > as then a else as 
    getMax _ as = as
    
-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr getSum 0
  where 
    getSum (DbNumber a) as = a + as
    getSum _ as = as

-- 5.    TODO - got the answer correctly, but check with Daniel for alternative solution using fold 
avgDb :: [DatabaseItem] -> Double
avgDb di = fromIntegral getSum / fromIntegral getLength
  where 
    getSum = sumDb di
    getLength = length $ filterDbNumber2 di