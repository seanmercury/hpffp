replaceWithP :: Char
replaceWithP = const 'p'

n = Nothing
w = Just "woohoo"
ave = Just "Ave"

lms :: List (Maybe (String))
lms = [n, w, ave]

(.) :: (b->c)->(a->b)->a->c
fmap :: Functor f => (m -> n) -> f m -> f n 
fmap :: Functor g => (x -> y) -> g x -> g y

(fmap . fmap) 
