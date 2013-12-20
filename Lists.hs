module Lists
  (
  last
  ,butLast
  ,elementAt
  ,length
  ,reverse
  ,isPalindrome
  ) where

import Prelude hiding (last, length, reverse)

last :: [a] -> a
last [x] = x
last (_:xs) = last xs

butLast :: [a] -> a
butLast [x,_] = x
butLast (_:xs) = butLast xs

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

isPalindrome:: Eq a => [a] -> Bool
isPalindrome x = (reverse x) == x
