module Lists
  (
  last
  ) where

import Prelude hiding (last)

last = head . reverse
