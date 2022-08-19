isSubseqOf :: (Eq a)
            => [a]
            -> [a]
            -> Bool

isSubseqOf [] [] = True
isSubseqOf x [] = False
isSubseqOf [] x = True
isSubseqOf (x:xs) b = elem x b && isSubseqOf xs b