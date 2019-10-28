module Main where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Lib

instance Arbitrary a => Arbitrary (Turducken a) where
  arbitrary = frequency
    [ (100, pure Empty)
    , (10, Node <$> arbitrary <*> arbitrary <*> arbitrary)
    , (1, Group <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

instance Eq a => EqProp (Turducken a) where
  (=-=) = eq

semiTrigger = undefined :: (Turducken (String, String, Int), Int)
monoTrigger = undefined :: (Turducken (String, String, Int), String)
funcTrigger = undefined :: Turducken (String, String, Int)
foldTrigger = undefined :: Turducken (Int, Int, [Int], Int, Int)
traverseTrigger = undefined :: Turducken (Int, Int, [Int])

main :: IO ()
main = do
  quickBatch $ semigroup semiTrigger
  quickBatch $ monoid monoTrigger
  quickBatch $ functor funcTrigger
  quickBatch $ foldable foldTrigger
  quickBatch $ traversable traverseTrigger
