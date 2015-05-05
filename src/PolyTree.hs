{-# LANGUAGE ExistentialQuantification #-}

-- Experimenting with different type enforced properties on binary trees.

module PolyTree where

-- Empty Node, usefull for the trees below
data Empty a = Empty deriving (Eq,Show)


-- Binary tree where every node can have a different type
data PolyTree a childTree1 val1 childTree2  val2 = PolyTree a (childTree1 val1) (childTree2 val2)

--testPoly = PolyTree 3 (PolyTree (\x -> x) (PolyTree ()) Empty) (PolyTree 'a' Empty)

--Balanced Binary Tree with a single type
data BalancedTree child a = BalancedTree a (child a) (child a)

balanced1 = BalancedTree 3 Empty Empty
balanced2 = BalancedTree 2 (balanced1) (balanced1)


--Balanced binary tree with polymorphic nodes
-- Still trying to figure out how to do this without existential types.
--data BalancedPoly a childStruct = BalancedPoly a childStruct  childStruct
data BalancedPoly child a = forall b c. BalancedPoly a (child b) (child c)

balancedPoly1Int = BalancedPoly 1 (Empty) (Empty)
balancedPoly1Func = BalancedPoly (\x -> x) (Empty) (Empty)

balancedPoly2 = BalancedPoly '4' (balancedPoly1Int) (balancedPoly1Int)


--data Bp a = forall b c. Bp a (Bp b) (Bp c) | Emptay
--b = Bp 3 (Bp (\x -> x) Emptay Emptay) (Emptay)
