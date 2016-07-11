{-# OPTIONS_GHC -XGADTs -XStandaloneDeriving #-}
module AVL (AVLTree(AvlEmpty, AvlTree), avlLeaf, avlTree, getHeight, insert, getRoot, getBalance, AVL.lookup, lookupApply, getLeft, getRight) where

import Data.Maybe

{------------------------------------------------------------------------------+
| sematic:                                                                     |
|                                                                              |
| AvlTree <node> <left subtree> <right subtree> <height>                       |
+------------------------------------------------------------------------------}
data AVLTree a where
  AvlTree :: (Show a, Ord a) => a -> AVLTree a -> AVLTree a -> Int -> AVLTree a
  AvlEmpty :: (Show a, Ord a) => AVLTree a

deriving instance Show (AVLTree a)

-- smart constructor for a leaf
avlLeaf :: (Ord a, Show a) => a -> AVLTree a
avlLeaf x = AvlTree x AvlEmpty AvlEmpty 1

-- get height of an AVLTree
getHeight :: AVLTree a -> Int
getHeight AvlEmpty = 0
getHeight (AvlTree _ _ _ h) = h

-- safe get root of an AVLTree
getRoot :: AVLTree a -> Maybe a
getRoot AvlEmpty = Nothing
getRoot (AvlTree x _ _ _) = Just x

-- unsafe get root. DO NOT CALL THIS PROCEDURE ON AN EMPTY AVLTREE
unsafeGetRoot :: AVLTree a -> a
unsafeGetRoot (AvlTree n _ _ _) = n

-- safe get left subtree of a tree
getLeft :: AVLTree a -> Maybe (AVLTree a)
getLeft AvlEmpty = Nothing
getLeft (AvlTree _ l _ _) = Just l

-- unsafe get left subtree of a tree
unsafeGetLeft :: AVLTree a -> AVLTree a
unsafeGetLeft (AvlTree _ l _ _) = l

-- safe get right subtree of a tree
getRight :: AVLTree a -> Maybe (AVLTree a)
getRight AvlEmpty = Nothing
getRight (AvlTree _ _ r _) = Just r

-- unsafe get right subtree of a tree
unsafeGetRight :: AVLTree a -> AVLTree a
unsafeGetRight (AvlTree _ _ r _) = r

-- get balance of an AVLTree
getBalance :: AVLTree a -> Int
getBalance AvlEmpty = 0
getBalance (AvlTree _ l r _) = (getHeight l) - (getHeight r)

-- compute the heigth difference of two AVLTrees
-- hDiff t0 t1 = (getHeight t0) - (getHeight t1)
hDiff :: AVLTree a -> AVLTree a -> Int
hDiff AvlEmpty AvlEmpty = 0
hDiff AvlEmpty (AvlTree _ _ _ h) = -h
hDiff (AvlTree _ _ _ h) AvlEmpty = h
hDiff (AvlTree _ _ _ h0) (AvlTree _ _ _ h1) = h0 - h1

-- functions to balance an unbalanced tree

-- do a right rotation
rotateRight :: AVLTree a -> AVLTree a
rotateRight (AvlTree x0 l0@(AvlTree x1 l1 r1 _) r0 _) = AvlTree x1 l1 r (1 + (max (getHeight l1) (getHeight r)))
  where r = AvlTree x0 r1 r0 (1 + (max (getHeight r1) (getHeight r0)))

-- do a left rotation
rotateLeft :: AVLTree a -> AVLTree a
rotateLeft (AvlTree x0 l0 r0@(AvlTree x1 l1 r1 _) _) = AvlTree x1 l r1 (1 + (max (getHeight l) (getHeight r1)))
  where l = AvlTree x0 l0 l1 (1 + (max (getHeight l0) (getHeight l1)))

{-
Insert an element in an AVLTree and do rotations if a node gets
unbalanced. The rotations are performed like shown in:

https://en.wikipedia.org/wiki/File:Tree_Rebalancing.gif

Rotations:

| case        | rotation    |
|-------------+-------------|
| left-left   | right       |
| left-right  | left, right |
| right-right | left        |
| right-left  | right, left |

-}
insert :: Ord a => a -> AVLTree a -> AVLTree a
insert x AvlEmpty = avlLeaf x
insert x t@(AvlTree y l r h)
  | x == y = t
  | x < y && (hDiff l' r) == 2 && x < (unsafeGetRoot l) = rotateRight $ AvlTree y l' r h
  | x < y && (hDiff l' r) == 2 && x > (unsafeGetRoot l) = rotateRight (AvlTree y (rotateLeft l') r h)
  | x > y && (hDiff l r') == -2 && x > (unsafeGetRoot r) = rotateLeft $ AvlTree y l r' h
  | x > y && (hDiff l r') == -2 && x < (unsafeGetRoot r) = rotateLeft $ AvlTree y l (rotateRight r') h
  | x < y = AvlTree y l' r (1 + (max (getHeight l') (getHeight r)))
  | x > y = AvlTree y l r' (1 + (max (getHeight l) (getHeight r')))
  where
    r' = insert x r
    l' = insert x l

-- turn a list to an AVLTree
avlTree :: (Ord a, Show a) => [a] -> AVLTree a
avlTree [] = AvlEmpty
avlTree (x:xs) = insert x (avlTree xs)

-- lookup a value 'x' in an AVLTree
lookup :: (Ord a) => a -> AVLTree a -> Maybe a
lookup x AvlEmpty = Nothing
lookup x (AvlTree y l r _)
  | x == y = Just y
  | x > y = AVL.lookup x r
  | x < y = AVL.lookup x l

-- apply a function 'f' to the nodes of an AVL tree and lookup a value
-- 'x' by comparing x with (f y) for every node 'y'
lookupApply :: (Ord a) => a -> (b -> a) -> AVLTree b -> Maybe b
lookupApply x _ AvlEmpty = Nothing
lookupApply x f (AvlTree y l r _)
  | x == fy = Just y
  | x > fy = lookupApply x f r
  | x < fy = lookupApply x f l
  where fy = (f y)
