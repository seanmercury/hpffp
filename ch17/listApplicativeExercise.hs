data List a = Nil | Cons a (List a) deriving (Eq, Show)


append :: List a -> List a -> List a
append Nil ys = ys 
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil


instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where 
  pure :: a -> List a
  pure a = Cons a Nil

  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) entireList@(Cons x xs) la = concat' $ fmap (\f -> fmap f la) entireList
  -- (<*>) entireList@(Cons aTob listaTob) lista = (fmap aTob lista) `append` (listaTob <*> lista)

flatMap :: (a -> List b) -> List a -> List b 
flatMap _ Nil = Nil
flatMap Nil _ = Nil
flatMap f as = concat' $ fmap f as

