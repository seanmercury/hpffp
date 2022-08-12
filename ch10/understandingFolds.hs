-- 2. Write out the evaluation steps for:
-- foldl (flip (*)) 1 [1..3]
-- f = (flip (*))
-- (((1 `f` 1) `f` 2) `f` 3)
-- ((1 `f` 2) `f` 3)
-- (2 `f` 3)
-- 6

-- 5.
-- a) foldr (++) ["woot", "WOOT", "woot"]
foldr (++) [] ["woot", "WOOT", "woot"]

-- b) foldr max [] "fear is the little death"
foldr max ' ' "fear is the little death"

-- c) foldr and True [False, True]
foldr (&&) True [False, True]

-- d) This one is more subtle than the previous. Can it ever return a different answer?
-- foldr (||) True [False, True]     NO
foldr (||) True [False, True]

-- e) foldl ((++) . show) "" [1..5]
foldr ((++) . show) "" [1..5]

-- f) foldr const 'a' [1..5]
-- (1 `const` (2 `const` (3 `const` (4 `const` (5 `const` 'a')))))
-- 
foldl const 'a' [1..5]
-- g) foldr const 0 "tacos"
foldl const 0 "tacos"
-- h) foldl (flip const) 0 "burritos"
foldr (flip const) 0 "burritos"
-- i) foldl (flip const) 'z' [1..5]
foldr (flip const) 'z' [1..5]