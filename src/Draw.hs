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
drawBorder b g@(Group a _ _ _) c = "|" ++ (rep (maxLength g) c) ++ "|"
 where
  halfLength n = (n - (length (show a))) `div` 2
  flank n x = [ x | _ <- [1 .. (halfLength n)] ]
  flanked s x = case b of
    True  -> s
    False -> [ x | _ <- [1 .. (length s)] ]
  rep n x = (flank n x) ++ (flanked (show a) x) ++ (flank n x)

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

height :: (Show a, Show b) => Turducken a b -> Int
height Empty            = 0
height (Node _ l r    ) = 2 + max (height l) (height r)
height (Group a ts l r) = 2 + maximum (fmap height (l : r : ts))

withHeight :: (Show a, Show b) => Turducken a b -> Turducken a (Int, String)
withHeight Empty = Empty
withHeight n@(Node x l r) =
  Node (height n, show x) (withHeight l) (withHeight r)
withHeight (Group a ts l r) =
  Group a (fmap withHeight ts) (withHeight l) (withHeight r)

allNodeHeights :: (Show a, Show b) => Turducken a b -> [(Int, String)]
allNodeHeights Empty = []
allNodeHeights n@(Node x l r) =
  (height n, show x) : allNodeHeights l ++ allNodeHeights r
allNodeHeights g@(Group y ts l r) =
  (height g, show y)
    : (  (foldr ((<>) . allNodeHeights) [] ts)
      ++ (allNodeHeights l)
      ++ (allNodeHeights r)
      )

maxLength :: (Show a, Show b) => Turducken a b -> Int
maxLength t = maximum $ fmap go (allNodeHeights t)
 where
  go (n, s) = ((totalHeight - n) * 3) + (length s) + 4
  totalHeight = maximum $ fmap justHeight wh
  justHeight (n, s) = n
  wh = withHeight t
