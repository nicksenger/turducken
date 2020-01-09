module Main where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Test.Hspec
import           Data.Foldable                  ( toList )

import           Lib

instance (Arbitrary a, Arbitrary b) => Arbitrary (Turducken a b) where
  arbitrary = frequency
    [ (100, pure Empty)
    , (10, Node <$> arbitrary <*> arbitrary <*> arbitrary)
    , (1, Group <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
    ]

instance (Eq a, Eq b) => EqProp (Turducken a b) where
  (=-=) = eq

semiTrigger = undefined :: (Turducken String (String, String, Int), Int)
monoTrigger = undefined :: (Turducken String (String, String, Int), String)
funcTrigger = undefined :: Turducken String (String, String, Int)
foldTrigger = undefined :: Turducken String (Int, Int, [Int], Int, Int)
traverseTrigger = undefined :: Turducken String (Int, Int, [Int])

main :: IO ()
main = do
  quickBatch $ semigroup semiTrigger
  quickBatch $ monoid monoTrigger
  quickBatch $ functor funcTrigger
  quickBatch $ applicative funcTrigger
  quickBatch $ monad funcTrigger
  quickBatch $ foldable foldTrigger
  quickBatch $ traversable traverseTrigger
  hspec $ do
    describe "Traversal order" $ do
      it "toList should give preorder with subturduckins before children" $ do
        toList
            (Node
              1
              (Group "Just a group"
                     [Node 2 Empty Empty, Node 3 Empty Empty]
                     (Node 4 Empty Empty)
                     (Node 5 Empty Empty)
              )
              (Group
                "Another group"
                [Node 6 Empty Empty, Node 7 Empty Empty]
                (Node 8
                      (Node 9 Empty Empty)
                      (Node 10 (Node 11 Empty Empty) Empty)
                )
                (Node 12 Empty Empty)
              )
            )
          `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
