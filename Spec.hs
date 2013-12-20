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
