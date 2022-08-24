myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : iterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = 
  case f b of
    Just (a, b) -> a : myUnfoldr f b
    Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (dothis f) x
  where dothis g a = Just (a, g a)

