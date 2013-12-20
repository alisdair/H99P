module Lists
  (
  last
  ,butLast
  ) where

import Prelude hiding (last)

last :: [a] -> a
last = head . reverse

butLast :: [a] -> a
butLast = head . tail . reverse
