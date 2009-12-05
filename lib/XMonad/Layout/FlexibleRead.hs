{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards, ViewPatterns #-}
module XMonad.Layout.FlexibleRead (
    flexibleRead,
    flexibleReadsPrec,
    flexibleReadInstance
    ) where

import XMonad (Window, LayoutClass(..))
import XMonad.StackSet (Workspace(..))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutCombinators
import XMonad.Util.TreeCorrection
import Data.Typeable
import Data.Tree
import Data.Maybe
import Data.Function
import Language.Haskell.TH
import Debug.Trace


-- | The "flexible read" layout modifier.
data FlexibleRead l a = FlexibleRead (l a)

instance (LayoutClass l a , ReadShowTree l a) => LayoutClass (FlexibleRead l) a where
    runLayout (Workspace i (FlexibleRead l) ms) r =
        fmap (fmap FlexibleRead) `fmap` runLayout (Workspace i l ms) r
    doLayout (FlexibleRead l) r s  = fmap (fmap FlexibleRead) `fmap` doLayout l r s
    emptyLayout (FlexibleRead l) r = fmap (fmap FlexibleRead) `fmap` emptyLayout l r
    handleMessage (FlexibleRead l) = fmap (fmap FlexibleRead) . handleMessage l
    description (FlexibleRead l)   = description l

instance (ReadShowTree l a) => Show (FlexibleRead l a) where
    show (FlexibleRead l) = show $ showTree l

flexibleRead :: (ReadShowTree l a) => l a -> FlexibleRead l a
flexibleRead = FlexibleRead

flexibleReadsPrec :: (ReadShowTree l a) =>
    FlexibleRead l a -> Int -> String -> [(FlexibleRead l a, [Char])]
flexibleReadsPrec (FlexibleRead l) i s =
    trace "\n\nflexread" $ case readsPrec i s of
        [(s', "")] -> trace (drawTree (fmap show s') ++ "\n\n" ++ drawTree (fmap show s'')) $
                [(FlexibleRead $ readTreeDef l s'', "")]
            where s'' = fmap unFst $ matchTrees (fmap Fst (showTree l)) (fmap Fst s')
        _          -> trace "fail" $ []

flexibleReadInstance :: Name -> Q [Dec]
flexibleReadInstance lay = do
    (VarI _ typ _ _) <- reify lay
    return
        [ InstanceD []
            (AppT (ConT (mkName "Read")) typ)
            [ ValD (VarP (mkName "readsPrec"))
                (NormalB (AppE
                    (VarE (mkName "XMonad.Layout.FlexibleRead.flexibleReadsPrec"))
                    (VarE lay)))
                []
            ]
        ]


newtype Fst = Fst (String, String)

unFst (Fst x) = x

instance Eq Fst where
    (==) = (==) `on` fst . unFst

instance Ord Fst where
    compare = compare `on` fst . unFst


class (Read (l a), LayoutClass l a) => ReadShowTree l a where
    readTreeDef :: l a -> Tree (String, String) -> l a
    readTreeDef def (Node (_, val) []) = fromMaybe def $ maybeRead val
    readTreeDef def _ = def

    showTree :: l a -> Tree (String, String)
    showTree a = Node (description a, show a) []

instance (Read (l a), LayoutClass l a) => ReadShowTree l a

instance (Read (m a), Show (m a), ReadShowTree l a, LayoutModifier m a) =>
        ReadShowTree (ModifiedLayout m l) a where

    readTreeDef ~(ModifiedLayout mod lay) (Node (_, val) [child]) =
        ModifiedLayout (fromMaybe mod $ maybeRead val) (readTreeDef lay child)
    readTreeDef def _ = def

    showTree (ModifiedLayout a b) = Node (head $ words a', a') [ showTree b ]
        where a' = show a

instance (ReadShowTree l1 a, ReadShowTree l2 a) => ReadShowTree (NewSelect l1 l2) a where
    showTree (NewSelect b l1 l2) = Node ("NewSelect", "") sf
        where l1' = sel b $ showTree l1
              l2'@(Node (l2x,l2y) l2z) = showTree l2
              sf = if l2x == "NewSelect"
                       then l1' : map sel2 l2z
                       else [ l1', sel (not b) l2' ]
              sel b' x = Node ("NewSelectBool", show b') [x]
              sel2 (Node _ x) | b = Node ("NewSelectBool", "False") x
              sel2 n = n

    readTreeDef ~(NewSelect _ l1 l2) (Node ("NewSelect", "") sf@(child : sfs@(child2:sfs2)))
        | (Node ("NewSelectBool", read -> childb) [childchild]) <- child,
          (Node ("NewSelectBool", _             ) [child2child]) <- child2 =
            let sel2 n@(Node ("NewSelectBool", _) x) | childb = Node ("NewSelectBool", "True") x
                sel2 n = n
                newl2 = if null sfs2
                    then readTreeDef l2 child2child
                    else readTreeDef l2 $ Node ("NewSelect", "") $ map sel2 sfs
            in NewSelect childb (readTreeDef l1 childchild) newl2
    readTreeDef def _ = def

maybeRead :: (Read a) => String -> Maybe a
maybeRead s =
    case reads s of
        [(x, "")] -> Just x
        _         -> Nothing
