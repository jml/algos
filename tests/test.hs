module Main (main) where

import Protolude

import Numeric.Natural
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  chapter1Spec <- testSpec "Chapter 1" chapter1
  pure $ testGroup "Algorithms Practice" [ chapter1Spec ]

chapter1 :: Spec
chapter1 = do
  describe "Counter-examples" $ do
    it "1-1 a + b can be less than min(a, b)" $ do
      (-1) + (-2) < min (-1) (-2 :: Int)
    it "1-2 a x b can be less than min(a, b)" $ do
      (-1) * 2 < min (-1) (2 :: Int)
    describe "1-5 Knapsack problem" $ do
      describe "First fit" $ do
        it "Sometimes works" $ do
          firstFit 5 [1, 1, 3] `shouldBe` Just [1, 1, 3]
          firstFit 5 [3, 1, 1] `shouldBe` Just [3, 1, 1]
        it "Can't find fitting solutions if big numbers come up first" $ do
          firstFit 5 [6, 1, 2, 2] `shouldBe` Nothing

firstFit :: Natural -> [Natural] -> Maybe [Natural]
firstFit goal sack =
  case dropWhile ((goal >) . fst) sums of
    [] -> Nothing  -- Not enough stuff in sack
    (y, result):_ ->
      case y `compare` goal of
        LT -> panic ("Impossible condition: " <> show y <> " < " <> show goal)
        EQ -> Just result
        GT -> Nothing -- This is a bad algorithm.
  where
    sums = zip (scanl (+) 0 sack) (inits sack)


{-|

1-3 Design / draw a road network with two points a & b such that the fastest
route is not the shortest route.

1-4 Design / draw a road network with two points a & b such that the shortest
route between a and b is not the route with the fewest turns.


-}
