dodgy :: (Num x, Eq x) => x -> x -> x
oneIsOne :: (Num x, Eq x) => x -> x
oneIsTwo :: (Num x, Eq x) => x -> x

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
