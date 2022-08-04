module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5
roundTrip2 :: (Show a, Read a) => a -> a
roundTrip2 = read . show

-- 6
roundTrip4 :: (Show a, Read b) => a -> b
roundTrip4 = read . show

main = do
  print (roundTrip 4) 
  print (roundTrip2 4) 
  print (roundTrip4 4::Int) 
  print (id 4)

