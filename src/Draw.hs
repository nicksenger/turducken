module Draw
  ( drawTurducken
  )
where

import           Lib

drawTurducken :: Turducken String -> String
drawTurducken = unlines . draw

draw :: Turducken String -> [String]
draw Empty        = []
draw (Node x l r) = x : (drawChildTurduckens (filter (not . isEmpty) [r, l]))
draw (Group ts l r) =
  [drawBorder ts '`']
    ++ (drawSubTurduckens ts)
    ++ [drawBorder ts '_']
    ++ (drawChildTurduckens (filter (not . isEmpty) [r, l]))

drawBorder :: [Turducken String] -> Char -> String
drawBorder ts c = "|" ++ (rep (maxGroupLength ts) c) ++ "|"
  where rep n x = [ x | _ <- [1 .. n] ]

drawSubTurduckens :: [Turducken String] -> [String]
drawSubTurduckens ts = case ts of
  []       -> []
  [t     ] -> "|" : shift "|  " "|  " (draw t)
  (t : ts) -> "|" : shift "|  " "|  " (draw t) ++ drawSubTurduckens ts

drawChildTurduckens :: [Turducken String] -> [String]
drawChildTurduckens ts = case ts of
  []       -> []
  [t     ] -> "|" : shift "`- " "   " (draw t)
  (t : ts) -> "|" : shift "`- " "|  " (draw t) ++ drawChildTurduckens ts

shift :: [a] -> [a] -> [[a]] -> [[a]]
shift first other = zipWith (++) (first : repeat other)

isEmpty :: Turducken a -> Bool
isEmpty Empty = True
isEmpty _     = False

height :: Turducken a -> Int
height Empty          = 0
height (Node  _  l r) = 1 + max (height l) (height r)
height (Group ts l r) = 2 + maximum (fmap height (l : r : ts))

withHeight :: Turducken a -> Turducken (Int, a)
withHeight Empty          = Empty
withHeight n@(Node x l r) = Node (height n, x) (withHeight l) (withHeight r)
withHeight (Group ts l r) =
  Group (fmap withHeight ts) (withHeight l) (withHeight r)

maxLength :: Turducken String -> Int
maxLength t = maximum $ fmap go (withHeight t)
 where
  go (n, s) = ((totalHeight - n) * 3) + (length s) + 4
  totalHeight = maximum $ fmap justHeight wh
  justHeight (n, s) = n
  wh = withHeight t

maxGroupLength :: [Turducken String] -> Int
maxGroupLength ts = maximum $ fmap maxLength ts
