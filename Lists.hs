module Lists
  (
  last
  ) where

import Prelude hiding (last)

last :: [a] -> a
last = head . reverse
