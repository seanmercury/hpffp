
data Quad =
  One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- How many different forms can this take?

eQuad :: Either Quad Quad
-- eQuad = ???     - prod 4 + 4 


prodQuad :: (Quad, Quad)  -- 4 * 4 
funcQuad :: Quad -> Quad  -- 4 ^ 4 
prodTBool :: (Bool, Bool, Bool)   -- 3 * 2
gTwo :: Bool -> Bool -> Bool    -- (2 ^ 2) ^ 2
fTwo :: Bool -> Quad -> Quad    -- (4 ^ 4) ^ 2