data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a))
        -> a
        -> BinaryTree b
unfold f a = 
  case f a of
    Just (a, b, c) -> Node (unfold f a) b (unfold f c)
    Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (f n) 0
  where f n i 
          | n > i = Just (i+1, i, i+1)
          | otherwise = Nothing 
    