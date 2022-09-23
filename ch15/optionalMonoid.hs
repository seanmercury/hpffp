module Optional where 

import Data.Monoid

data Optional a = 
  Nada
  | Only a
  deriving (Eq, Show)


-- TODO ask Daniel
-- why this has to be included when semigroup is super class of monoid anyway
-- 
instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only a) Nada = Only a
  (<>) Nada (Only a) = Only a
  (<>) (Only a) (Only b) = Only (a <> b)
  
-- ch15/optionalMonoid.hs:15:10: error:
--     • Could not deduce (Semigroup (Optional a))
--         arising from the superclasses of an instance declaration
--       from the context: Monoid a
instance Monoid a => Monoid (Optional a) where
  mempty = Nada 
  
  
