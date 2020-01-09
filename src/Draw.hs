module Draw
  ( drawTurducken
  )
where

import           Lib

drawTurducken :: (Show a, Show b) => Turducken a b -> String
drawTurducken = unlines . draw

draw :: (Show a, Show b) => Turducken a b -> [String]
draw Empty        = []
draw (Node x l r) = (show x) : (drawChildTurduckens (filter (not . isEmpty) [r, l]))
draw (Group a ts l r) =
  [drawBorder ts '`']
    ++ (drawSubTurduckens ts)
    ++ [drawBorder ts '_']
    ++ (drawChildTurduckens (filter (not . isEmpty) [r, l]))

drawBorder :: (Show a, Show b) => [Turducken a b] -> Char -> String
drawBorder ts c = "|" ++ (rep (maxGroupLength ts) c) ++ "|"
  where rep n x = [ x | _ <- [1 .. n] ]

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

height :: Turducken a b -> Int
height Empty          = 0
height (Node  _  l r) = 1 + max (height l) (height r)
height (Group a ts l r) = 2 + maximum (fmap height (l : r : ts))

withHeight :: Turducken a b -> Turducken a (Int, b)
withHeight Empty          = Empty
withHeight n@(Node x l r) = Node (height n, x) (withHeight l) (withHeight r)
withHeight (Group a ts l r) =
  Group a (fmap withHeight ts) (withHeight l) (withHeight r)

maxLength :: (Show a, Show b) => Turducken a b -> Int
maxLength t = maximum $ fmap go (withHeight t)
 where
  go (n, s) = ((totalHeight - n) * 3) + (length (show s)) + 4
  totalHeight = maximum $ fmap justHeight wh
  justHeight (n, s) = n
  wh = withHeight t

maxGroupLength :: (Show a, Show b) => [Turducken a b] -> Int
maxGroupLength ts = maximum $ fmap maxLength ts
