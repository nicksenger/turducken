module Draw
  ( drawTurducken
  )
where

import           Lib
import           Data.Foldable                  ( toList )

drawTurducken :: (Show a, Show b) => Turducken a b -> String
drawTurducken = unlines . draw

draw :: (Show a, Show b) => Turducken a b -> [String]
draw Empty = []
draw (Node x l r) =
  (show x) : (drawChildTurduckens (filter (not . isEmpty) [r, l]))
draw g@(Group a ts l r) =
  [drawBorder True g '`']
    ++ (drawSubTurduckens ts)
    ++ [drawBorder False g '_']
    ++ (drawChildTurduckens (filter (not . isEmpty) [r, l]))

drawBorder :: (Show a, Show b) => Bool -> Turducken a b -> Char -> String
drawBorder showTitle g@(Group a _ _ _) c =
  "|" ++ (flank (maxSubHeight g) c) ++ "|"
 where
  halfLength n = (n - (length (show a))) `div` 2
  pad n x = [ x | _ <- [1 .. (halfLength n)] ]
  padded s x = case showTitle of
    True  -> s
    False -> [ x | _ <- [1 .. (length s)] ]
  flank n x = (pad n x) ++ (padded (show a) x) ++ (pad n x)

drawSubTurduckens :: (Show a, Show b) => [Turducken a b] -> [String]
drawSubTurduckens ts = case ts of
  []       -> []
  [t     ] -> "|" : shift "|  " "|  " (draw t)
  (t : ts) -> "|" : shift "|  " "|  " (draw t) ++ drawSubTurduckens ts

drawChildTurduckens :: (Show a, Show b) => [Turducken a b] -> [String]
drawChildTurduckens ts = case ts of
  []       -> []
  [t     ] -> "|" : shift "`- " "   " (draw t)
  (t : ts) -> "|" : shift "`- " "|  " (draw t) ++ drawChildTurduckens ts

shift :: [a] -> [a] -> [[a]] -> [[a]]
shift first other = zipWith (++) (first : repeat other)

isEmpty :: Turducken a b -> Bool
isEmpty Empty = True
isEmpty _     = False

height :: (Show a, Show b) => Turducken a b -> (Int, Int)
height Empty = (0, 0)
height (Node x l r) =
  (maximum [length $ show x, fst hl, fst hr], 4 + (max (snd hl) (snd hr)))
 where
  hl = height l
  hr = height r
height g@(Group a ts l r) = case addHeight hg > addHeight hl of
  True -> case addHeight hg > addHeight hr of
    True  -> hg
    False -> hr
  False -> case addHeight hl > addHeight hr of
    True  -> hl
    False -> hr
 where
  hg        = (0, length (drawBorder False g '-'))
  hl        = height l
  hr        = height r
  addHeight = \(x, y) -> x + y

maxSubHeight :: (Show a, Show b) => Turducken a b -> Int
maxSubHeight g@(Group _ ts _ _) =
  maximum $ fmap (\(x, y) -> x + y) $ fmap height ts
