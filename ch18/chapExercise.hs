{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Test.QuickCheck (arbitrary, Arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Data.Semigroup
import Control.Applicative (liftA3)
import Control.Monad

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

-- Identity


-- Composition

-- Homomorphism

-- Interchange

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) NopeDotJpg _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq




  

data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap :: (a1 -> a2) -> (BahEither b) a1 -> (BahEither b) a2
  fmap f (PLeft x) = PLeft (f x)
  fmap _ (PRight y) = PRight y

instance Applicative (BahEither b) where
  pure :: a -> (BahEither b) a
  pure x = PLeft x
  
  (<*>) :: (BahEither b) (a1 -> a2) -> (BahEither b) a1 -> (BahEither b) a2
  (<*>) (PLeft f) (PLeft x) = PLeft (f x)
  (<*>) (PRight x) (PLeft _) = PRight x
  (<*>) (PLeft _) (PRight x) = PRight x
  (<*>) (PRight x1) (PRight x2) = PRight x1 -- must choose left-biased impl or else ap =-= (<*>) fails

instance Monad (BahEither b) where
  return = pure
  (>>=) (PLeft x) f = f x
  (>>=) (PRight x) _ = PRight x


instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (PRight b)), (9, return (PLeft a))]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq




newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where 
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where 
  return = pure
  (>>=) (Identity x) f = f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq



data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where 
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) $ fmap f as





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
  pure a = Cons a Nil

  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) entireList@(Cons x xs) la = concat' $ fmap (\f -> fmap f la) entireList
  -- (<*>) entireList@(Cons aTob listaTob) lista = (fmap aTob lista) `append` (listaTob <*> lista)

instance Monad List where 
  return = pure
  (>>=) entireList f = concat' $ fmap f entireList

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (9, Cons <$> arbitrary <*> arbitrary)]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq




j :: Monad m => m (m a) -> m a
j = join 

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = fmap f ma

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a ma mab = ma >>= \a -> mab >>= \ab -> return $ ab a

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (a:la) fab = fab a >>= \temp -> meh la fab >>= \temp1 -> return $ temp : temp1

flipType :: (Monad m) => [m a] -> m [a]
flipType ma = meh ma id



main :: IO ()
main = do
  -- putStrLn "Nope"
  -- quickBatch $ functor (undefined :: Nope (Int, Int, Int))
  -- quickBatch $ applicative (undefined :: Nope (Int, Int, Int))
  -- quickBatch $ monad (undefined :: Nope (Int, Int, Int))
  -- _ <- getChar

  putStrLn "BahEither"
  quickBatch $ functor (undefined :: BahEither Int (Int, Int, Int))
  quickBatch $ applicative (undefined :: BahEither Int (Int, Int, Int))
  quickBatch $ monad (undefined :: BahEither Int (Int, Int, Int))
  _ <- getChar

  -- putStrLn "Identity"
  -- quickBatch $ functor (undefined :: Identity (Int, Int, Int))
  -- quickBatch $ applicative (undefined :: Identity (Int, Int, Int))
  -- quickBatch $ monad (undefined :: Identity (Int, Int, Int))
  -- _ <- getChar

  -- putStrLn "List"
  -- quickBatch $ functor (undefined :: List (Int, Int, Int))
  -- quickBatch $ applicative (undefined :: List (Int, Int, Int))
  -- quickBatch $ monad (undefined :: List (Int, Int, Int))
  -- _ <- getChar

  pure ()



---- Notes below

-- allTuples :: [a] -> [b] -> [(a,b)]
-- allTuples xs [] = []
-- allTuples xs (y:ys) = fmap (\x -> (x,y)) xs <> allTuples xs ys

-- allTuples' :: [a] -> [b] -> [(a,b)]
-- allTuples' xs ys = do
--   x <- xs
--   y <- ys
--   pure (x,y)

-- allTuples'' :: [a] -> [b] -> [(a,b)]
-- allTuples'' = [(x,y) | x <- xs, y <- ys]

-- type Logged a = ([String], a)

-- myFunc1 :: Int -> Logged Int
-- myFunc1 n = (["Adding 5"], n + 5)

-- myFunc2 :: Int -> Logged Int
-- myFunc2 n = (["Adding 5 to " <> show n], n + 5)

-- -- different signature from `myFunc1`, but contains equivalent data.
-- myFunc3 :: Logged (Int -> Int)
-- myFunc3 = (["Adding 5"], \n -> n + 5)

-- -- is it possible to write a function with this signature
-- -- that contains equivalend data to `myFunc2`?
-- -- i.e., can we log the function's input?
-- myFunc4 :: Logged (Int -> Int)
-- myFunc4 = (undefined "here's the only place i can log, but you don't have the `n`", \n -> n + 5)
-- -- it's impossible to write a function that's equivalent to `myFunc2` with this signature


-- (=<<) :: Monad       s =>   (a -> s b) -> s a -> s b
-- --                                               ^ depends on s from second arg and s from _result_ of first arg (which depends on a)
-- (<*>) :: Applicative s => s (a ->   b) -> s a -> s b
-- --                                               ^ depends on s from first arg and s from second, but not on a


apLeftBiased :: Monad m => m (a -> b) -> m a -> m b
apLeftBiased mf ma = do
  f <- mf
  a <- ma
  pure (f a)

apRightBiased :: Monad m => m (a -> b) -> m a -> m b
apRightBiased mf ma = do
  a <- ma
  f <- mf
  pure (f a)




