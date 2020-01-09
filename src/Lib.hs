module Lib where

import           Control.Monad                  ( ap )

data Turducken a b =
  Empty |
  Node b (Turducken a b) (Turducken a b) |
  Group a [Turducken a b] (Turducken a b) (Turducken a b)
  deriving (Eq, Show)

instance (Monoid a) => Semigroup (Turducken a b) where
  Empty <> t     = t
  t     <> Empty = t
  (Node x l1 r1) <> (Node y l2 r2) =
    Group mempty [(Node x l1 r1), (Node y l2 r2)] Empty Empty
  (Node  x  l1 r1) <> (Group a ts l2 r2) = Group a ((Node x l1 r1) : ts) l2 r2
  (Group a ts l1 r1) <> (Node  x  l2 r2) = Group a (ts ++ [(Node x l2 r2)]) l1 r1
  (Group a1 ts1 l1 r1) <> (Group a2 ts2 l2 r2) =
    Group (a1 <> a2) (ts1 ++ ts2) (l1 <> l2) (r1 <> r2)

instance (Monoid a) => Monoid (Turducken a b) where
  mempty = Empty

instance Functor (Turducken a) where
  fmap _ Empty          = Empty
  fmap f (Node  x  l r) = Node (f x) (fmap f l) (fmap f r)
  fmap f (Group a ts l r) = Group a ((fmap . fmap) f ts) (fmap f l) (fmap f r)

instance (Monoid a) => Applicative (Turducken a) where
  pure x = Node x Empty Empty
  (<*>) = ap

instance (Monoid a) => Monad (Turducken a) where
  return = pure
  Empty          >>= f = Empty
  (Node x l1 r1) >>= f = case (f x) of
    Empty          -> Empty
    Node  y  l2 r2 -> Node y (l2 <> (l1 >>= f)) (r2 <> (r1 >>= f))
    Group a ts l2 r2 -> Group a ts (l2 <> (l1 >>= f)) (r2 <> (r1 >>= f))
  (Group a ts l r) >>= f = Group a (fmap (>>= f) ts) (l >>= f) (r >>= f)

instance Foldable (Turducken a) where
  foldMap _ Empty        = mempty
  foldMap f (Node x l r) = (f x) <> (foldMap f l) <> (foldMap f r)
  foldMap f (Group a ts l r) =
    (foldr ((<>) . (foldMap f)) mempty ts) <> (foldMap f l) <> (foldMap f r)

instance Traversable (Turducken a) where
  traverse _ Empty = pure Empty
  traverse f (Node x l r) =
    Node <$> (f x) <*> (traverse f l) <*> (traverse f r)
  traverse f (Group a ts l r) =
    Group a <$> ((traverse . traverse) f ts) <*> (traverse f l) <*> (traverse f r)
