import Control.Applicative
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Data.Semigroup

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where 
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs 
                in take 3000 l
          ys' = let (ZipList' l) = ys 
                in take 3000 l

instance (Semigroup a) => Semigroup (ZipList' a) where
  (<>) (ZipList' as) (ZipList' bs) = ZipList' (zipWith (\a b -> a <> b) as bs)


-- (<>) [1,2,3] [4,5,6]  
-- A: [1 <> 4, 2 <> 5, 3 <> 6]
-- zipWith (\a b -> a <> b) as bs


instance (Monoid a) => Monoid (ZipList' a) where
  mempty = ZipList' (repeat mempty)

instance Functor ZipList' where 
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where 
  pure :: a -> ZipList' a
  pure x = ZipList' (repeat x)

  -- (<*>) (pure id) v = v
   
  
  (<*>) :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
  (<*>) _ (ZipList' []) = ZipList' []
  (<*>) (ZipList' []) _ = ZipList' []
  (<*>) (ZipList' (x:xs)) (ZipList' (la:las)) = ZipList' (x la : dothis)
                                                  where ZipList' dothis = (ZipList' xs) <*> (ZipList' las)

  -- x :: [a], la :: [a]
instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary


main :: IO ()
main = do
  hspec $ do
    describe "zipList` Semigroup" $ do
      it "should combine elements component-wise : Sum" $ do
        let lhs = ZipList' [1,2,3 :: Sum Integer]
            rhs = ZipList' [4,5,6]
            ans = ZipList' [1 <> 4, 2 <> 5, 3 <> 6]
        lhs <> rhs `shouldBe` ans
      it "should combine elements component-wise : Product" $ do
        let lhs = ZipList' [1,2,3 :: Product Integer]
            rhs = ZipList' [4,5,6]
            ans = ZipList' [1 <> 4, 2 <> 5, 3 <> 6]
        lhs <> rhs `shouldBe` ans
      it "should combine elements component-wise : Max" $ do
        let lhs = ZipList' [1,2,3 :: Max Integer]
            rhs = ZipList' [4,5,6]
            ans = ZipList' [1 <> 4, 2 <> 5, 3 <> 6]
        lhs <> rhs `shouldBe` ans
      it "should have identity" $ do
        let lhs = ZipList' [1,2,3 :: Sum Integer]
            rhs = mempty
            ans = ZipList' [1 <> mempty,2 <> mempty,3 <> mempty]
        lhs <> rhs `shouldBe` ans
      -- pure id <*> x = x
      it "should have applicative identity" $ do    
        let lhs = pure id
            rhs = ZipList' [1,2,3 :: Sum Integer]
            ans = ZipList' [1,2,3 :: Sum Integer]
        lhs <*> rhs `shouldBe` ans
  quickBatch $ functor (undefined :: ZipList' (Int, Int, Int))
  quickBatch $ applicative (undefined :: ZipList' (Int, Int, Int))
  -- TODO Daniel - applicative fails on identity & composition check. 
