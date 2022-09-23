newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Show (Mem s a) where 
  show (Mem _) = "Mem"

instance (Semigroup a) => Semigroup (Mem s a) where
  (<>) :: Mem s a -> Mem s a -> Mem s a
  (<>) (Mem f) (Mem g) = Mem (\s0 -> 
    let (a1, s1) = f s0
        (a2, s2) = g s1
    in (a1 <> a2, s2))

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))
  mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0 
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int)) 
  print $ rmleft == runMem f' 0 
  print $ rmright == runMem f' 0