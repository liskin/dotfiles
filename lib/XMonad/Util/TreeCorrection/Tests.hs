module XMonad.Util.TreeCorrection.Tests where

import XMonad.Util.TreeCorrection.Internal

import Data.Tree
import Data.List
import Data.Function
import Debug.Trace
import Test.QuickCheck -- tested with quickcheck 2


prop_paths_unpaths t = t == unpaths t (paths t)
    where types = (t :: Tree Int)

-- Given two trees with a same structure, unpaths should return the second one
-- unchanged.
prop_paths_unpaths2 t = ((==) `on` (fmap unFst)) t2 (unpaths t1 (paths t2))
    where
        types = (t :: Tree Int)
        t1 = fmap (\a -> Fst (a, a)) t
        t2 = fmap (\a -> Fst (a, a + 5)) t

-- If only the order of paths changes, matchPaths gets it back exactly.
prop_matchPaths_sort xs = all (uncurry (==)) $ matchPaths xs (sort xs)
    where types = (xs :: [[Int]])

prop_matchPaths_reverse xs = all (uncurry (==)) $ matchPaths xs (reverse xs)
    where types = (xs :: [[Int]])

-- If the order of subtrees changes, we still retain the data.
prop_matchTrees_order t1 t2 t3 = ((==) `on` (fmap unFst)) tn' (matchTrees tn to)
    where
        tOld = Node 1 [ t1, t2, t3 ]
        tNew = Node 1 [ t2, t3, t1 ]
        tn = fmap (\a -> Fst (a, a)) tNew
        to = fmap (\a -> Fst (a, a + 5)) tOld
        tn' = fmap (\a -> Fst (a, a + 5)) tNew

-- If the modifier in the root is changed, we retain all other data.
-- This one is falsifiable if the new or the old modifier is somewhere deeper
-- in the tree as well.
prop_matchTrees_mod t1 t2 t3 = (treeDebugEq `on` (fmap unFst)) tn' (matchTrees tn to)
    where
        sub = map (fmap (\a -> Fst (a, a))) [ t1, t2, t3 ]
        tn = Node (Fst (123456789, 0)) sub
        to = Node (Fst (1234567890, 0)) $ map (fmap (\(Fst (a, b)) -> Fst (a, b + 5))) sub
        tn' = Node (Fst (123456789, 0)) $ map (fmap (\(Fst (a, b)) -> Fst (a, b + 5))) sub

-- If a layout is added to the end, we retain all old data.
prop_matchTrees_add t1 t2 t3 t4 = ((==) `on` (fmap unFst)) tn' (matchTrees tn to)
    where
        subO = map (fmap (\a -> Fst (a, a))) [ t1, t2, t3 ]
        subN = map (fmap (\a -> Fst (a, a))) [ t4 ]
        tn = Node (Fst (0, 0)) $ subO ++ subN
        to = Node (Fst (0, 5)) $ map (fmap (\(Fst (a, b)) -> Fst (a, b + 5))) subO
        tn' = Node (Fst (0, 5)) $ map (fmap (\(Fst (a, b)) -> Fst (a, b + 5))) subO ++ subN

{- Does not hold, since intersect [1,1] [1] = [1,1].
prop_intersect' xs ys = intersect' xs' ys' == intersect xs' ys'
    where types = (xs :: [Int], ys :: [Int])
          xs' = sort xs
          ys' = sort ys
-}


-- | Arbitrary instance for trees.
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized tree
        where
            tree 0 = arbitrary >>= return . flip Node []
            tree n | n > 0 = do
                nsub <- resize 5 $ arbitrarySizedIntegral
                val <- arbitrary
                subs <- vectorOf nsub $ tree (n `div` nsub)
                return $ Node val subs

treeDebugEq :: (Show a, Eq a) => Tree a -> Tree a -> Bool
treeDebugEq a b = if a == b then True else tr False
    where
        tr x = trace ("\n" ++ drawTree (fmap show a) ++ "\n/=\n\n" ++ drawTree (fmap show b) ++ "\n\n") x
