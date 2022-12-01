{-# LANGUAGE ScopedTypeVariables #-}

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b 
  fmap f (StateT sm) = StateT $ (\c -> fmap (\(a, c') -> (f a, c')) (sm c))   -- got this from Daniel's functor exercise. ;)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT (\c -> pure (a, c))
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  -- (<*>) (StateT smf) (StateT sma) = StateT $ (<*>) <$> smf  <*> sma
  -- (<*>) (StateT smf) (StateT sma) = StateT (\s0 -> 
  --   let mf = smf s0

  --   in do 
  --         (f', y') <- mf
  --         (a', s1 :: s) <- sma y'
          
  --         pure $ (f' a', s1)
  --   )   

  (<*>) (StateT smf) (StateT sma) = StateT (\s0 -> do 
    (f, s1) <- smf s0
    (a, s2) <- sma s1
    
    pure (f a, s2)
  )           

-- (<*>) :: m (a -> b) ->  m a -> m b
  -- 3 ways to create m
  -- 1. use fmap
  -- 2. pure
  -- 3. use <*> operator
  -- 4. >>= operator

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (>>=) (StateT sma) f = StateT $ \s -> do
    v <- sma s
    runStateT (f (fst v) ) s