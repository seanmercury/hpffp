{-# LANGUAGE ScopedTypeVariables #-}

data List x = Nil | Cons x (List x) deriving (Eq, Show)

instance Functor List where
  fmap :: forall a b . (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons foo bar) =
    let _ = foo :: a
        _ = bar :: List a
    in Cons (f foo) (fmap f bar)



append :: List a -> List a -> List a
append Nil ys = ys 
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil


instance Applicative List where
  pure :: a -> List a
  pure foo = Cons foo Nil

  -- (<*>) :: forall a b . List (a -> b) -> List a -> List b
  -- (<*>) Nil _ = Nil
  -- (<*>) _ Nil = Nil
  -- (<*>) entireList@(Cons foo bar) baz = Cons _ _
    -- let _ = foo :: (a -> b)
    --     _ = bar :: List (a -> b)
    --     _ = baz :: List a
    --     _ = entireList :: List (a -> b)
    -- in _

  (<*>) :: forall a b . List (a -> b) -> List a -> List b
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  -- (<*>) entireList@(Cons aTob listaTob) lista = concat' (fmap (\f -> fmap f lista) entireList)
  (<*>) entireList@(Cons aTob listaTob) lista = (fmap aTob lista) `append` (listaTob <*> lista)
 
flatMap :: (a -> List b) -> List a -> List b 
flatMap f as = concat' (fmap f as)