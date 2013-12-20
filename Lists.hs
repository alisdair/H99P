module Lists
  (
  last
  ,butLast
  ) where

import Prelude hiding (last)

last :: [a] -> a
last [x] = x
last (_:xs) = last xs

butLast :: [a] -> a
butLast [x,_] = x
butLast (_:xs) = butLast xs
