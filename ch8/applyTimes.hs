applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 5 (+1) 5
