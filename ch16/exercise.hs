data Quant a b = 
  Finance
  | Desk a 
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

{-# LANGUAGE FlexibleInstances #-}

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a = DaWrappa (f a) (g a)
  
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a = 
  Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a listA) = Cons (f a) (fmap f listA) 

data GoatLord a = 
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor (GoatLord) where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c) 

data TalkToMe a = 
  Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str a) = Print str (f a)
  fmap f (Read sa) = Read (fmap f sa)

-----------------
-- Daniel
-----------------


{-# LANGUAGE InstanceSigs #-}

-- 1.
newtype F c a = F ( c -> a )
instance Functor (F c) where
  fmap :: (a -> b) -> F c a -> F c b
  fmap f (F ca) = F (\c -> f (ca c))

-- 2.
newtype M c a = M ( c -> (a, c) )
instance Functor (M c) where
    fmap f (M ca) = M ( \c -> let (a, c') = ca c
                              in (f a, c') )

-- 3.
newtype R c a = R ( (a -> c) -> c )
instance Functor (R c) where
  fmap :: (a -> b) -> R c a -> R c b
  -- fmap f (R rc) = R ( \a -> let h = rc g
  --                               g = 
  --                             in g r  )
  fmap f (R rc) = R (\g -> rc (g . f))

-- 4.
newtype F' c t a = F' ( c -> t a )   -- F' f c
instance Functor t => Functor (F' c t) where
  fmap :: (a -> b) -> F' c t a -> F' c t b
  fmap f (F' fct) = F' (\c -> fmap f (fct c))

-- 5.
newtype M' c t a = M' ( c -> t (a, c) )
instance Functor t => Functor (M' c t) where
  fmap :: (a -> b) -> M' c t a -> M' c t b
  -- fmap f (M' ct) = M' (\c -> let t (a, c') = (ct c)
  --                             in fmap (f a, c') )  
  fmap f (M' ct) = M' (\c -> (fmap (\(a, c') -> (f a, c'))) (ct c))

-- 6.
newtype R' c t a = R' ( (a -> t c) -> t c )
instance Functor (R' c t) where
  fmap :: (a -> b) -> R' c t a -> R' c t b
  fmap f (R' ct) = R' (\btotc -> ct (btotc . f))


data Tuple a b = Tuple a b deriving (Show)
instance Functor (Tuple a) where
  fmap :: (x -> y) -> Tuple a x -> Tuple a y
  fmap f (Tuple a b) = Tuple a (f b)
