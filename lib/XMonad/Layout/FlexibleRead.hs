{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Tree
import Data.Maybe
import Language.Haskell.TH


-- | The "flexible read" layout modifier.
data FlexibleRead l a = FlexibleRead (l a)

-- | The 'LayoutClass' instance for our modifier. We cannot use 'FlexibleRead'
-- as a normal modifier in 'ModifiedLayout' since the 'Show' and 'Read'
-- instances of 'ModifiedLayout' can not be overlapped.
instance (LayoutClass l a, ReadShowTree (l a)) => LayoutClass (FlexibleRead l) a where
    runLayout (Workspace i (FlexibleRead l) ms) r =
        fmap (fmap FlexibleRead) `fmap` runLayout (Workspace i l ms) r
    doLayout (FlexibleRead l) r s  = fmap (fmap FlexibleRead) `fmap` doLayout l r s
    emptyLayout (FlexibleRead l) r = fmap (fmap FlexibleRead) `fmap` emptyLayout l r
    handleMessage (FlexibleRead l) = fmap (fmap FlexibleRead) . handleMessage l
    description (FlexibleRead l)   = description l

-- | A non-default 'Show' instance for 'FlexibleRead'. This is what makes the
-- output parseable even if the layout changes.
instance (ReadShowTree (l a)) => Show (FlexibleRead l a) where
    show (FlexibleRead l) = show $ showTree l

-- | Apply the 'FlexibleRead' modifier to a layout.
flexibleRead :: (ReadShowTree (l a)) => l a -> FlexibleRead l a
flexibleRead = FlexibleRead

-- | A helper function for the implementation of 'Read' instance. This is more
-- or less a reverse of 'show' that additionally applies the tree correction
-- algorithm.
flexibleReadsPrec :: (ReadShowTree (l a)) =>
    FlexibleRead l a -> Int -> String -> [(FlexibleRead l a, [Char])]
flexibleReadsPrec (FlexibleRead l) i s | [(s', "")] <- readsPrec i s =
    [(FlexibleRead $ readTreeDef l $ matchTreesFst (showTree l) s', "")]
flexibleReadsPrec _ _ _ = []

-- | Given a name of your layout variable, this defines the 'Read' instance
-- for your particular layout type and with your particular layout as a
-- default for non-readable parts of the input.
--
-- Usage:
-- > {-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
-- > $( flexibleReadInstance 'myLayout )
flexibleReadInstance :: Name -> Q [Dec]
flexibleReadInstance lay = do
    (VarI _ typ _ _) <- reify lay
    inst <- [d| readsPrec = flexibleReadsPrec $(varE lay) |]
    return [ InstanceD [] (AppT (ConT (mkName "Read")) typ) inst ]


-- | A type class for types that can be serialized to and from trees of
-- @(String, String)@. Useful for data types that have a hierarchical
-- structure, like 'ModifiedLayout'.
class ReadShowTree a where
    -- | Read a value from a tree, taking the unreadable parts from a
    -- specified default value.
    readTreeDef :: a -> Tree (String, String) -> a
    -- | Show a value into a tree.
    showTree :: a -> Tree (String, String)

-- | The 'ReadShowTree' instance for a generic layout. This shows into a leaf.
instance (Read (l a), LayoutClass l a) => ReadShowTree (l a) where
    readTreeDef def (Node (_, val) []) = fromMaybe def $ maybeRead val
    readTreeDef def _ = def

    showTree a = Node (description a, show a) []

-- | The 'ReadShowTree' instance for a layout modifier. This shows into a node
-- with a single child, the layout being modified.
instance (Read (m a), Show (m a), ReadShowTree (l a), LayoutModifier m a) =>
        ReadShowTree (ModifiedLayout m l a) where

    readTreeDef ~(ModifiedLayout mod lay) (Node (_, val) [child]) =
        ModifiedLayout (fromMaybe mod $ maybeRead val) (readTreeDef lay child)
    readTreeDef def _ = def

    showTree (ModifiedLayout a b) = Node (head $ words a', a') [ showTree b ]
        where a' = show a

-- | The 'ReadShowTree' instance for the 'NewSelect' combinator. This shows
-- into a node with as many children as layout choices in this select.
instance (ReadShowTree (l1 a), ReadShowTree (l2 a)) => ReadShowTree (NewSelect l1 l2 a) where
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
        | (Node ("NewSelectBool", childb') [childchild]) <- child,
          childb <- read childb',
          (Node ("NewSelectBool", _      ) [child2child]) <- child2 =
            let sel2 n@(Node ("NewSelectBool", _) x) | childb = Node ("NewSelectBool", "True") x
                sel2 n = n
                newl2 = if null sfs2
                    then readTreeDef l2 child2child
                    else readTreeDef l2 $ Node ("NewSelect", "") $ map sel2 sfs
            in NewSelect childb (readTreeDef l1 childchild) newl2
    readTreeDef def _ = def

-- | A convenient 'read' that does not 'error' on failure.
maybeRead :: (Read a) => String -> Maybe a
maybeRead s =
    case reads s of
        [(x, "")] -> Just x
        _         -> Nothing
