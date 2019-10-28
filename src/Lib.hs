module Lib where

data Turducken a =
  Empty |
  Node a (Turducken a) (Turducken a) |
  Group [Turducken a] (Turducken a) (Turducken a)
  deriving (Eq, Show)

instance Semigroup (Turducken a) where
  Empty <> t     = t
  t     <> Empty = t
  (Node x left1 right1) <> (Node y left2 right2) =
    Group [(Node x left1 right1), (Node y left2 right2)] Empty Empty
  (Node x left1 right1) <> (Group sub left2 right2) =
    Group ((Node x left1 right1) : sub) left2 right2
  (Group sub left1 right1) <> (Node x left2 right2) =
    Group (sub ++ [(Node x left2 right2)]) left1 right1
  (Group sub1 left1 right1) <> (Group sub2 left2 right2) =
    Group (sub1 ++ sub2) (left1 <> left2) (right1 <> right2)

instance Monoid (Turducken a) where
  mempty = Empty

instance Functor Turducken where
  fmap _ Empty               = Empty
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
  fmap f (Group sub left right) =
    Group ((fmap . fmap) f sub) (fmap f left) (fmap f right)

instance Foldable Turducken where
  foldMap _ Empty = mempty
  foldMap f (Node x left right) =
    (f x) <> (foldMap f left) <> (foldMap f right)
  foldMap f (Group xs left right) =
    (foldr ((<>) . (foldMap f)) mempty xs)
      <> (foldMap f left)
      <> (foldMap f right)

instance Traversable Turducken where
  traverse _ Empty = pure Empty
  traverse f (Node x left right) =
    Node <$> (f x) <*> (traverse f left) <*> (traverse f right)
  traverse f (Group xs left right) =
    Group
      <$> ((traverse . traverse) f xs)
      <*> (traverse f left)
      <*> (traverse f right)
