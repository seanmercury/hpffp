data Size = Small | Medium | Large deriving (Eq, Show)

data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer = 
  Mini | Mazda | Tata
  deriving (Eq, Show)

data Airline =
  PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Medium
doge2 = Plane TakeYourChancesUnited Large

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars [] = [False]
areCars (x:xs)  
  | isCar x = True : areCars xs
  | otherwise = False : areCars xs 

areCars2 :: [Vehicle] -> [Bool]
areCars2 = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car (m::Manufacturer) _) = m

