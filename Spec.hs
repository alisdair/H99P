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
