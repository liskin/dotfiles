{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  XMonad.Hooks.LayoutHistory
-- Description :  Keep history of layouts per workspace.
-- Copyright   :  (c) 2022 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
--
-- Keeps track of history of layouts per workspace, and provides utilities to
-- switch to the recent (previous) layouts.
--
module XMonad.Hooks.LayoutHistory (
    -- * Usage
    -- $usage
    keepLayoutHistory,
    jumpToPreviousLayout,
    toggleLayout,

    -- * Low-level interface to the stored history
    recordCurrentLayoutHistory,
    recordWorkspaceLayoutHistory,
    updateWorkspaceLayoutHistory,
    updateLayoutHistory,
    getWorkspaceLayoutHistory,
    getLayoutHistory,
    ) where

import Control.DeepSeq
import Data.Maybe
import qualified Data.Map.Strict as M

import XMonad
import XMonad.Prelude (delete, isPrefixOf)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleConf as XC
import qualified XMonad.Util.ExtensibleState as XS

-- ---------------------------------------------------------------------
-- $usage
--
-- TODO
-- mention X.L.Renamed

newtype LayoutHistoryHooked = LayoutHistoryHooked ()
  deriving newtype (Semigroup)

-- | Install hooks to record layout history.
keepLayoutHistory :: XConfig l -> XConfig l
keepLayoutHistory =
    XC.once (\cfg -> cfg{ logHook = logHook cfg <> recordCurrentLayoutHistory }) LayoutHistoryHooked

-- | Switch current workspace to the previous layout.
jumpToPreviousLayout :: X ()
jumpToPreviousLayout = do
    cur <- gets $ W.workspace . W.current . windowset
    h <- getWorkspaceLayoutHistory (W.tag cur)
    case filter (description (W.layout cur) /=) h of
        l : _ -> sendMessage (JumpToLayout l)
        _ -> mempty

-- | Switch current workspace to the given layout, or if that layout is
-- currently active, to the previous layout.
toggleLayout :: String -> X ()
toggleLayout l = do
    curl <- gets $ description . W.layout . W.workspace . W.current . windowset
    if l == curl then jumpToPreviousLayout else sendMessage (JumpToLayout l)


-- ---------------------------------------------------------------------
-- Low-level interface to the stored history

newtype LayoutHistory = LayoutHistory{ fromLayoutHistory :: M.Map WorkspaceId [String] }
    deriving (Show, Read)

instance ExtensionClass LayoutHistory where
    extensionType = PersistentExtension
    initialValue = LayoutHistory mempty

-- | Record the current layout into current workspace's layout history.
recordCurrentLayoutHistory :: X ()
recordCurrentLayoutHistory = do
    cur <- gets $ W.workspace . W.current . windowset
    recordWorkspaceLayoutHistory cur

-- | Record the current layout into the given workspace's layout history.
-- (May be useful when switching layouts on non-current workspaces.)
recordWorkspaceLayoutHistory :: WindowSpace -> X ()
recordWorkspaceLayoutHistory w =
    updateWorkspaceLayoutHistory (W.tag w) $ \h -> if [l] `isPrefixOf` h then h else l : l `delete` h
  where
    l = description . W.layout $ w

updateWorkspaceLayoutHistory :: WorkspaceId -> ([String] -> [String]) -> X ()
updateWorkspaceLayoutHistory w f = updateLayoutHistory $ M.alter f' w
  where
    f' = force . Just . f . fromMaybe []

updateLayoutHistory :: (M.Map WorkspaceId [String] -> M.Map WorkspaceId [String]) -> X ()
updateLayoutHistory f = XS.modify' $ LayoutHistory . f . fromLayoutHistory

getWorkspaceLayoutHistory :: WorkspaceId -> X [String]
getWorkspaceLayoutHistory w = M.findWithDefault [] w <$> getLayoutHistory

getLayoutHistory :: X (M.Map WorkspaceId [String])
getLayoutHistory = XS.gets fromLayoutHistory

-- TODO: gc (restrict the map to existing workspaces)
