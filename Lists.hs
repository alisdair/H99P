module Lists
  (
  last
  ,butLast
  ,elementAt
  ) where

import Prelude hiding (last)

last :: [a] -> a
last [x] = x
last (_:xs) = last xs

butLast :: [a] -> a
butLast [x,_] = x
butLast (_:xs) = butLast xs

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)
