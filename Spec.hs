import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lists

main :: IO ()
main = hspec $ do
  describe "Lists.last" $ do
    it "returns the last element of a list" $ do
      Lists.last [1 .. 45] `shouldBe` (45 :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (Lists.last []) `shouldThrow` anyException

  describe "Lists.butLast" $ do
    it "returns the second-last element of a list" $ do
      Lists.butLast [1, 2, 3] `shouldBe` (2 :: Int)

    it "throws an exception if used with a unit list" $ do
      evaluate (Lists.butLast [1]) `shouldThrow` anyException

  describe "Lists.elementAt" $ do
    it "returns the first element when given 1" $ do
      Lists.elementAt [1] 1 `shouldBe` (1 :: Int)

    it "returns the third element when given 3" $ do
      Lists.elementAt [5,6,7,8] 3 `shouldBe` (7 :: Int)

    it "throws an exception if the index is out of bounds" $ do
      evaluate (Lists.elementAt [1] 2) `shouldThrow` anyException

  describe "Lists.length" $ do
    it "returns 0 for empty list" $ do
      Lists.length [] `shouldBe` (0 :: Int)

    it "returns 1 for unit list" $ do
      Lists.length [5] `shouldBe` (1 :: Int)

    it "returns 5 for [1,3,5,7,9]" $ do
      Lists.length [1,3,5,7,9] `shouldBe` (5 :: Int)

  describe "Lists.reverse" $ do
    it "returns [] for empty list" $ do
      Lists.reverse [] `shouldBe` ([] :: [Int])

    it "returns list for unit list" $ do
      Lists.reverse [5] `shouldBe` ([5] :: [Int])

    it "returns [2,1] for [1,2]" $ do
      Lists.reverse [1,2] `shouldBe` ([2,1] :: [Int])

    it "returns [3,2,1] for [1,2,3]" $ do
      Lists.reverse [1,2,3] `shouldBe` ([3,2,1] :: [Int])

  describe "Lists.isPalindrome" $ do
    it "returns false for [1,2,3]" $ do
      Lists.isPalindrome [1,2,3] `shouldBe` (False :: Bool)

    it "returns true for 'madamimadam'" $ do
      Lists.isPalindrome "madamimadam" `shouldBe` (True :: Bool)

    it "returns true for [1,2,4,8,16,8,4,2,1]" $ do
      Lists.isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` (True :: Bool)

  describe "Lists.flatten" $ do
    it "flattens empty list" $ do
      Lists.flatten (List []) `shouldBe` ([] :: [Int])

    it "flattens unit list" $ do
      Lists.flatten (Elem 5) `shouldBe` ([5] :: [Int])

    it "flattens nested list" $ do
      Lists.flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` ([1,2,3,4,5] :: [Int])

  describe "Lists.compress" $ do
    it "compresses repeated elements" $ do
      Lists.compress "aaaabccaadeeee" `shouldBe` ("abcade" :: String)

  describe "Lists.pack" $ do
    it "packs consecutive duplicates" $ do
      Lists.pack "aaaabccaadeeee" `shouldBe` (["aaaa", "b", "cc", "aa", "d", "eeee"] :: [String])

  describe "Lists.rle" $ do
    it "performs run-length encoding" $ do
      Lists.rle "aaaabccaadeeee" `shouldBe` ([(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] :: [(Int, Char)])

  describe "Lists.rle2" $ do
    it "performs modified run-length encoding" $ do
      Lists.rle2 "aaaabccaadeeee" `shouldBe` ([Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] :: [RunLength Char])

  describe "Lists.decodeModified" $ do
    it "decodes modified run-length encoded list" $ do
      Lists.decodeModified ([Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] :: [RunLength Char]) `shouldBe` ("aaaabccaadeeee" :: String)

  describe "Lists.dupli" $ do
    it "duplicates each element of the list" $ do
      Lists.dupli "abcd" `shouldBe` ("aabbccdd" :: String)

  describe "Lists.repli" $ do
    it "replicates each element a given number of times" $ do
      Lists.repli "abcd" 3 `shouldBe` ("aaabbbcccddd" :: String)

  describe "Lists.dropEvery" $ do
    it "drops no elements" $ do
      Lists.dropEvery "abcde" 10 `shouldBe` ("abcde" :: String)

    it "drops every 3rd element" $ do
      Lists.dropEvery "abcdefghik" 3 `shouldBe` ("abdeghk" :: String)

    it "drops every 2nd element" $ do
      Lists.dropEvery "abcde" 2 `shouldBe` ("ace" :: String)

    it "drops every element" $ do
      Lists.dropEvery "abcde" 1 `shouldBe` ("" :: String)

  describe "Lists.split" $ do
    it "splits a list at an index" $ do
      Lists.split "abcdefghik" 3 `shouldBe` (("abc", "defghik") :: (String, String))

    it "splits with zero index" $ do
      Lists.split "abcdefghik" 0 `shouldBe` (("", "abcdefghik") :: (String, String))

  describe "Lists.slice" $ do
    it "slices empty list when called with 1 0" $ do
      Lists.slice "abcde" 1 0 `shouldBe` ("" :: String)

    it "slices first element when called with 1 1" $ do
      Lists.slice "abcde" 1 1 `shouldBe` ("a" :: String)

    it "slices sub-list" $ do
      Lists.slice "abcde" 2 4 `shouldBe` ("bcd" :: String)
