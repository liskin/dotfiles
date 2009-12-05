{-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module XMonad.Util.TreeCorrection (
    matchTrees
    ) where

import Data.Tree
import Data.Typeable
import Data.Data
import Data.Ord
import Data.List
import Data.Maybe


paths :: Tree a -> [[a]]
paths (Node a sf)
    | null sf   = [[a]]
    | otherwise = map (a :) $ concatMap paths sf

unpaths :: Eq a => Tree a -> [[a]] -> Tree a
unpaths t ps = let ([], t') = f ps t in t'
    where
        f ps (Node a sf) = (ps', Node (find' a $ head ps) sf')
            where
                (ps', sf') = if null sf
                                then (tail ps, [])
                                else mapAccumL f (map (\\ [a]) ps) sf

        find' a s = fromMaybe a $ find (a ==) s


matchPath :: (Eq a, Ord a) => [a] -> [[a]] -> [a]
matchPath _ [] = []
matchPath (sort -> p) s = fst $ maximumBy (comparing snd) $ map f s
    where
        f x = (x, (length $ intersect' p x', negate $ length $ (p \\ x) ++ (x \\ p)))
            where x' = sort x


matchPaths :: (Eq a, Ord a) => [[a]] -> [[a]] -> [([a], [a])]
matchPaths [] _ = []
matchPaths (p:ps) qs = (p, q) : matchPaths ps (qs \\ [q])
    where q = matchPath p qs

prop_matchPaths_sort xs = all (uncurry (==)) $ matchPaths xs (sort xs)
    where types = (xs :: [[Int]])

prop_matchPaths_reverse xs = all (uncurry (==)) $ matchPaths xs (reverse xs)
    where types = (xs :: [[Int]])


matchTrees :: (Eq a, Ord a) => Tree a -> Tree a -> Tree a
matchTrees tn to = unpaths tn (map snd ps)
    where ps = matchPaths (paths tn) (paths to)


intersect' :: Ord a => [a] -> [a] -> [a]
intersect' s@(x:xs) t@(y:ys) =
    case x `compare` y of
        EQ -> x : intersect' xs ys
        LT -> intersect' xs t
        GT -> intersect' s ys
intersect' _ _ = []

{- Does not hold, since intersect [1,1] [1] = [1,1].
prop_intersect' xs ys = intersect' xs' ys' == intersect xs' ys'
    where types = (xs :: [Int], ys :: [Int])
          xs' = sort xs
          ys' = sort ys
-}


-- Example.
data T = A Int | B Int | C Int | D Int | E Int | F Int | G Int | H Int
    deriving (Typeable, Data, Show)

instance Eq T where
    a == b = toConstr a == toConstr b

instance Ord T where
    compare = comparing (show . toConstr)

tree1 =
    Node (A 4) [
        Node (B 8) [
            Node (C 15) [],
            Node (D 1) [ Node (E 2) [] ],
            Node (E 1) []
        ]
    ]

tree2 =
    Node (G 3) [
        Node (A 1) [
            Node (B 1) [
                Node (E 1) [],
                Node (C 1) [],
                Node (D 1) [ Node (E 3) [] ]
            ]
        ]
    ]

tree3 =
    Node 1 []
