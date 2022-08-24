-- As natural as any
-- competitive bodybuilder
data Nat = Zero
          | Succ Nat
          deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + natToInteger a

integerToNat :: Integer -> Maybe Nat
integerToNat a = if a < 0 then Nothing else Just (dothis a)
  where dothis a
          | a == 0 = Zero
          | a > 0 = Succ (dothis (a - 1))