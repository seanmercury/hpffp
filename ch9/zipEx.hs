myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (a:ax) (b:bx) = (a, b) : myZip ax bx

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f _ [] = []
myZipWith f [] _ = []
myZipWith f (a:ax) (b:bx) = f a b : myZipWith f ax bx 

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 a b = myZipWith (,) a b -- TODO ??????????