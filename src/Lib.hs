module Lib where

import           Control.Monad                  ( ap )

data Turducken a =
  Empty |
  Node a (Turducken a) (Turducken a) |
  Group [Turducken a] (Turducken a) (Turducken a)
  deriving (Eq, Show)

instance Semigroup (Turducken a) where
  Empty <> t     = t
  t     <> Empty = t
  (Node x l1 r1) <> (Node y l2 r2) =
    Group [(Node x l1 r1), (Node y l2 r2)] Empty Empty
  (Node  x  l1 r1) <> (Group ts l2 r2) = Group ((Node x l1 r1) : ts) l2 r2
  (Group ts l1 r1) <> (Node  x  l2 r2) = Group (ts ++ [(Node x l2 r2)]) l1 r1
  (Group ts1 l1 r1) <> (Group ts2 l2 r2) =
    Group (ts1 ++ ts2) (l1 <> l2) (r1 <> r2)

instance Monoid (Turducken a) where
  mempty = Empty

instance Functor Turducken where
  fmap _ Empty          = Empty
  fmap f (Node  x  l r) = Node (f x) (fmap f l) (fmap f r)
  fmap f (Group ts l r) = Group ((fmap . fmap) f ts) (fmap f l) (fmap f r)

instance Applicative Turducken where
  pure x = Node x Empty Empty
  (<*>) = ap

instance Monad Turducken where
  return = pure
  Empty          >>= f = Empty
  (Node x l1 r1) >>= f = case (f x) of
    Empty          -> Empty
    Node  y  l2 r2 -> Node y (l2 <> (l1 >>= f)) (r2 <> (r1 >>= f))
    Group ts l2 r2 -> Group ts (l2 <> (l1 >>= f)) (r2 <> (r1 >>= f))
  (Group ts l r) >>= f = Group (fmap (>>= f) ts) (l >>= f) (r >>= f)

instance Foldable Turducken where
  foldMap _ Empty        = mempty
  foldMap f (Node x l r) = (f x) <> (foldMap f l) <> (foldMap f r)
  foldMap f (Group ts l r) =
    (foldr ((<>) . (foldMap f)) mempty ts) <> (foldMap f l) <> (foldMap f r)

instance Traversable Turducken where
  traverse _ Empty = pure Empty
  traverse f (Node x l r) =
    Node <$> (f x) <*> (traverse f l) <*> (traverse f r)
  traverse f (Group ts l r) =
    Group <$> ((traverse . traverse) f ts) <*> (traverse f l) <*> (traverse f r)
