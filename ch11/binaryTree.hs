data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a
        => a
        -> BinaryTree a
        -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
  1
  (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
  2
  (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup OK!"
  else error "test failed!"





preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (preorder left) ++ [a] ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (preorder left) ++ (preorder right) ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."


foldTree :: (a -> b -> b)
            -> b
            -> BinaryTree a
            -> b
foldTree f b Leaf = b
foldTree f b (Node left a right) = foldTree f b (f a (foldTree f b left) )