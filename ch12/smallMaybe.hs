isJust :: Maybe a -> Bool
isJust Nothing = False
isJust otherwise = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing otherwise = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f (Just a) = f a
mayybee b f Nothing = b

fromMaybe :: a -> Maybe a -> a
fromMaybe a (Just b) = b
fromMaybe a Nothing = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:as) = catMaybes as
catMaybes ((Just a):as) = a : catMaybes as


-- TODO : Ask Daniel
-- flipMaybe :: [Maybe a] -> Maybe [a]
-- flipMaybe [] = Nothing
-- flipMaybe (Nothing:as) = Nothing
-- flipMaybe (