{-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-} -- a need with ViewPatterns
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
unpaths t ps = let ([], t') = f id ps t in t'
    where
        f g ps (Node a sf) = (ps', Node (find' a $ g $ head ps) sf')
            where
                (ps', sf') = if null sf
                                then (tail ps, [])
                                else mapAccumL (f ((\\ [a]) . g)) ps sf

        find' a s = fromMaybe a $ find (a ==) s


matchPath :: (Eq a, Ord a) => [a] -> [[a]] -> [a]
matchPath _ [] = []
matchPath (sort -> p) s = fst $ maximumBy' (comparing snd) $ map f s
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

maximumBy'               :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ []          =  error "maximumBy': empty list"
maximumBy' cmp xs        =  foldl1 maxBy xs
                         where
                            maxBy x y = case cmp x y of
                                        LT -> y
                                        _  -> x

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

tree4 = Node {rootLabel = ("WorkspaceDir","WorkspaceDir \"~\""), subForest = [Node {rootLabel = ("NewSelect",""), subForest = [Node {rootLabel = ("NewSelectBool","True"), subForest = [Node {rootLabel = ("Named","Named \"tiled\""), subForest = [Node {rootLabel = ("AvoidStruts","AvoidStruts (fromList [U,D,R,L])"), subForest = [Node {rootLabel = ("LayoutHints","LayoutHints (0.5,0.5)"), subForest = [Node {rootLabel = ("ConfigurableBorder","ConfigurableBorder Never []"), subForest = [Node {rootLabel = ("MouseResizableTile","MRT {nmaster = 1, masterFrac = 1 % 2, leftFracs = [], rightFracs = [], draggers = [], focusPos = 0, numWindows = 0, isMirrored = False}"), subForest = []}]}]}]}]}]},Node {rootLabel = ("NewSelectBool","False"), subForest = [Node {rootLabel = ("Named","Named \"mtiled\""), subForest = [Node {rootLabel = ("AvoidStruts","AvoidStruts (fromList [U,D,R,L])"), subForest = [Node {rootLabel = ("LayoutHints","LayoutHints (0.5,0.5)"), subForest = [Node {rootLabel = ("ConfigurableBorder","ConfigurableBorder Never []"), subForest = [Node {rootLabel = ("MouseResizableTile","MRT {nmaster = 1, masterFrac = 1 % 2, leftFracs = [], rightFracs = [], draggers = [], focusPos = 0, numWindows = 0, isMirrored = True}"), subForest = []}]}]}]}]}]},Node {rootLabel = ("NewSelectBool","False"), subForest = [Node {rootLabel = ("Named","Named \"tab\""), subForest = [Node {rootLabel = ("AvoidStruts","AvoidStruts (fromList [U,D,R,L])"), subForest = [Node {rootLabel = ("LayoutHints","LayoutHints (0.5,0.5)"), subForest = [Node {rootLabel = ("ConfigurableBorder","ConfigurableBorder Never []"), subForest = [Node {rootLabel = ("Decoration","Decoration   (Theme {activeColor = \"#999999\", inactiveColor = \"#666666\", urgentColor = \"#FFFF00\", activeBorderColor = \"#FFFFFF\", inactiveBorderColor = \"#BBBBBB\", urgentBorderColor = \"##00FF00\", activeTextColor = \"#FFFFFF\", inactiveTextColor = \"#BFBFBF\", urgentTextColor = \"#FF0000\", fontName = \"-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*\", decoWidth = 200, decoHeight = 20, windowTitleAddons = []}) (Tabbed Top WhenPlural)"), subForest = [Node {rootLabel = ("Simplest","Simplest"), subForest = []}]}]}]}]}]}]},Node {rootLabel = ("NewSelectBool","False"), subForest = [Node {rootLabel = ("Named","Named \"float\""), subForest = [Node {rootLabel = ("WA","WA True True []"), subForest = [Node {rootLabel = ("SimplestFloat","SF"), subForest = []}]}]}]},Node {rootLabel = ("NewSelectBool","False"), subForest = [Node {rootLabel = ("Named","Named \"full\""), subForest = [Node {rootLabel = ("LayoutHints","LayoutHints (0.0,0.0)"), subForest = [Node {rootLabel = ("WithBorder","WithBorder 0 []"), subForest = [Node {rootLabel = ("Full","Full"), subForest = []}]}]}]}]}]}]}

tree5 = Node {rootLabel = ("WorkspaceDir","WorkspaceDir \"~\""), subForest = [Node {rootLabel = ("NewSelect",""), subForest = [Node {rootLabel = ("NewSelectBool","True"), subForest = [Node {rootLabel = ("Named","Named \"tab\""), subForest = [Node {rootLabel = ("AvoidStruts","AvoidStruts (fromList [U,D,R,L])"), subForest = [Node {rootLabel = ("LayoutHints","LayoutHints (0.5,0.5)"), subForest = [Node {rootLabel = ("ConfigurableBorder","ConfigurableBorder Never []"), subForest = [Node {rootLabel = ("Decoration","Decoration   (Theme {activeColor = \"#999999\", inactiveColor = \"#666666\", urgentColor = \"#FFFF00\", activeBorderColor = \"#FFFFFF\", inactiveBorderColor = \"#BBBBBB\", urgentBorderColor = \"##00FF00\", activeTextColor = \"#FFFFFF\", inactiveTextColor = \"#BFBFBF\", urgentTextColor = \"#FF0000\", fontName = \"-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*\", decoWidth = 200, decoHeight = 20, windowTitleAddons = []}) (Tabbed Top WhenPlural)"), subForest = [Node {rootLabel = ("Simplest","Simplest"), subForest = []}]}]}]}]}]}]},Node {rootLabel = ("NewSelectBool","False"), subForest = [Node {rootLabel = ("Named","Named \"tiled\""), subForest = [Node {rootLabel = ("AvoidStruts","AvoidStruts (fromList [U,D,R,L])"), subForest = [Node {rootLabel = ("LayoutHints","LayoutHints (0.5,0.5)"), subForest = [Node {rootLabel = ("ConfigurableBorder","ConfigurableBorder Never []"), subForest = [Node {rootLabel = ("MouseResizableTile","MRT {nmaster = 1, masterFrac = 1 % 2, leftFracs = [], rightFracs = [], draggers = [], focusPos = 0, numWindows = 0, isMirrored = False}"), subForest = []}]}]}]}]}]},Node {rootLabel = ("NewSelectBool","False"), subForest = [Node {rootLabel = ("Named","Named \"mtiled\""), subForest = [Node {rootLabel = ("AvoidStruts","AvoidStruts (fromList [U,D,R,L])"), subForest = [Node {rootLabel = ("LayoutHints","LayoutHints (0.5,0.5)"), subForest = [Node {rootLabel = ("ConfigurableBorder","ConfigurableBorder Never []"), subForest = [Node {rootLabel = ("MouseResizableTile","MRT {nmaster = 1, masterFrac = 1 % 2, leftFracs = [], rightFracs = [], draggers = [], focusPos = 0, numWindows = 0, isMirrored = True}"), subForest = []}]}]}]}]}]},Node {rootLabel = ("NewSelectBool","False"), subForest = [Node {rootLabel = ("Named","Named \"float\""), subForest = [Node {rootLabel = ("WA","WA True True []"), subForest = [Node {rootLabel = ("SimplestFloat","SF"), subForest = []}]}]}]},Node {rootLabel = ("NewSelectBool","False"), subForest = [Node {rootLabel = ("Named","Named \"full\""), subForest = [Node {rootLabel = ("LayoutHints","LayoutHints (0.0,0.0)"), subForest = [Node {rootLabel = ("WithBorder","WithBorder 0 []"), subForest = [Node {rootLabel = ("Full","Full"), subForest = []}]}]}]}]}]}]}
