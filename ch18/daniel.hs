{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import Test.QuickCheck (arbitrary, Arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes hiding (apply)
import Data.Monoid
import Data.Semigroup
import Control.Applicative (liftA3)
import Control.Monad

data Func a b = Func (a -> b)

apply :: Func a b -> a -> b
apply (Func f) x = f x

instance Functor (Func a) where
  fmap :: (b1 -> b2) -> Func a b1 -> Func a b2
  fmap g (Func f) = Func $ g . f

instance Applicative (Func a) where
  pure :: b -> Func a b
  pure x = Func $ const x

  (<*>) :: Func a (b1 -> b2) -> Func a b1 -> Func a b2
  (<*>) (Func f) (Func g) = Func $ (\temp -> f temp (g temp))    -- f :: a -> b1 -> b2, g :: a -> b1

instance Monad (Func a) where
  return :: b -> Func a b
  return = pure

  -- (>>=) :: Func a b1 -> (b1 -> Func a b2) -> Func a b2
  (>>=) :: forall a b1 b2. Func a b1 -> (b1 -> Func a b2) -> Func a b2
  -- (>>=) (Func f) g = (\temp -> g (f temp))   --    f :: a -> b1, g :: b1 -> Func a b2 
  (>>=) (Func f) g = Func (\temp -> apply (g (f temp)) temp)

-- use `where` and `let` blocks!
-- give type signatures to intermediate values
-- make heavy use of `_`.
-- when in doubt, pattern match!
-- after you solve it, cover up your scratch work

-- the more polymorphic you make your function, the more likely your implementation will be correct.
