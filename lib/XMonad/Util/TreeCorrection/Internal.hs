module XMonad.Util.TreeCorrection.Internal where

import Data.Tree
import Data.Ord
import Data.List
import Data.Maybe
import Data.Function


-- | Paths from root to all leafs.
paths :: Tree a -> [[a]]
paths (Node a sf)
    | null sf   = [[a]]
    | otherwise = map (a :) $ concatMap paths sf

-- | Reconstruct a tree from a sample tree and a list of paths. (This is
-- actually only useful with non-standard 'Eq' instances.)
unpaths :: Eq a => Tree a -> [[a]] -> Tree a
unpaths t ps = let ([], t') = f id ps t in t'
    where
        f g ps (Node a sf) = (ps', Node (find' a $ g $ head ps) sf')
            where
                (ps', sf') = if null sf
                                then (tail ps, [])
                                else mapAccumL (f ((\\ [a]) . g)) ps sf

        find' a s = fromMaybe a $ find (a ==) s


-- | Return the closest match for a path. The heuristic used is: 1) the path
-- that has more common elements; if that is equal 2) the path that differs in
-- less elements.
matchPath :: (Eq a, Ord a) => [a] -> [[a]] -> [a]
matchPath _ [] = []
matchPath p' s = fst $ maximumBy' (comparing snd) $ map f s
    where
        p = sort p'
        f x = (x, (length $ intersect' p x', negate $ length $ (p \\ x) ++ (x \\ p)))
            where x' = sort x

-- | Match two lists of paths. The first path from the first lists gets its
-- closest match, that is deleted from the set of available paths, and this
-- repeats until all paths get what is left to them.
matchPaths :: (Eq a, Ord a) => [[a]] -> [[a]] -> [([a], [a])]
matchPaths [] _ = []
matchPaths (p:ps) qs = (p, q) : matchPaths ps (qs \\ [q])
    where q = matchPath p qs

-- | For trees @tNew@ and @tOld@, this function tries to return a tree with a
-- structure of @tNew@ and contents of @tOld@. The structure is defined by the
-- 'Eq' instance of the elements, hence wrapping in 'Fst' is probably desired.
matchTrees :: (Eq a, Ord a) => Tree a -> Tree a -> Tree a
matchTrees tn to = unpaths tn (map snd ps)
    where ps = matchPaths (paths tn) (paths to)


-- | 'intersect' optimized for ordered lists.
intersect' :: Ord a => [a] -> [a] -> [a]
intersect' s@(x:xs) t@(y:ys) =
    case x `compare` y of
        EQ -> x : intersect' xs ys
        LT -> intersect' xs t
        GT -> intersect' s ys
intersect' _ _ = []

-- | The same as 'maximumBy', but returns the /first/ greatest element, not the
-- last.
maximumBy'               :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ []          =  error "maximumBy': empty list"
maximumBy' cmp xs        =  foldl1 maxBy xs
                         where
                            maxBy x y = case cmp x y of
                                        LT -> y
                                        _  -> x


-- | A newtype wrapper for pairs with Eq and Ord instances considering only
-- the first element.
newtype Fst a b = Fst (a, b) deriving (Show, Read)

unFst (Fst x) = x

instance (Eq a) => Eq (Fst a b) where
    (==) = (==) `on` fst . unFst

instance (Ord a) => Ord (Fst a b) where
    compare = compare `on` fst . unFst
