lefts' :: [Either a b] -> [a]
lefts' = foldr dothis []
  where  
    dothis (Left a) as = a : as
    dothis (Right b) as = as

rights' :: [Either a b] -> [b]
rights' = foldr dothis []
  where  
    dothis (Right a) as = a : as
    dothis (Left b) as = as

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr dothis ([], [])
  where  
    dothis (Left a) (as, bs) = (a : as, bs)
    dothis (Right b) (as, bs) = (as, b : bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' f (Left a) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Left a) = Nothing
eitherMaybe'' f (Right b) = Just (either' id f (Right b))