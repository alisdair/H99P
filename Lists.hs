module Lists
  (
  last
  ,butLast
  ,elementAt
  ,length
  ,reverse
  ,isPalindrome
  ,flatten
  ,NestedList(Elem, List)
  ,compress
  ,pack
  ,rle
  ,RunLength(Single, Multiple)
  ,rle2
  ,decodeModified
  ) where

import Prelude hiding (last, length, reverse)
import Data.List (group)

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

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: Eq a => [a] -> [a]
compress = foldr compress' []
  where
    compress' x [] = [x]
    compress' x xs
      | head xs == x = xs
      | otherwise    = x : xs

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:ys):(pack zs)
  where
    (ys,zs) = span (== x) xs

rle :: Eq a => [a] -> [(Int, a)]
rle = map count' . pack
  where
    count' xs = (length xs, head xs)

data RunLength a = Single a | Multiple Int a
   deriving (Show, Eq)
rle2 :: Eq a => [a] -> [RunLength a]
rle2 = map foo . rle
  where
    foo (1, x) = Single x
    foo (i, x) = Multiple i x

decodeModified :: [RunLength a] -> [a]
decodeModified = concatMap foo
  where
    foo :: RunLength a -> [a]
    foo (Single x) = [x]
    foo (Multiple i x) = replicate i x
