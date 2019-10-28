module Lib where

data Turducken a =
    Empty |
    Leaf a [Turducken a] |
    Node a [Turducken a] (Turducken a) (Turducken a)
