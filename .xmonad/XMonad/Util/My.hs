{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      :  XMonad.Util.My
-- Copyright   :  (c) 2021 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
--
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
-- Stability   :  experimental
-- Portability :  unknown
--
-- My xmonad utils to declutter xmonad.hs
--
module XMonad.Util.My where

import Control.Monad
import Data.List.Split (splitOneOf)
import Data.Maybe
import Data.Monoid
import System.Directory (getCurrentDirectory)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WorkspaceNames
import XMonad.Layout.Inspect
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.SubLayouts
import XMonad.Layout.WorkspaceDir
import XMonad.Util.Run
import XMonad.Util.Ungrab
import qualified XMonad.Util.PureX as P

{-# NOINLINE myHome #-}
myHome :: String
myHome = unsafePerformIO $ getEnv "HOME"

jumpToLayout :: String -> X ()
jumpToLayout = sendMessage . JumpToLayout

-- | Update workspace name (if empty) to current directory
curDirToWorkspacename :: X ()
curDirToWorkspacename = do
    name <- getCurrentWorkspaceName
    when (isNothing name) $ do
        dir <- io getCurrentDirectory
        when (dir /= myHome) $ do
            setCurrentWorkspaceName $ last $ splitOneOf "/" dir

-- | Select an X action from a list using a @rofi@ prompt and do it.
runSelectedAction :: String -> [(String, X ())] -> X ()
runSelectedAction prompt actions = do
    unGrab
    out <- lines <$> runProcessWithInput "rofi" ["-dmenu", "-p", prompt] (unlines $ map fst actions)
    case out of
        [sel] -> maybe (pure ()) id (sel `lookup` actions)
        _ -> pure ()

-- | Select a git repo and change current workspace directory to it.
changeDirRofiGit :: X ()
changeDirRofiGit = do
    unGrab
    out <- lines <$> runProcessWithInput "rofi-git-all-repos" [] ""
    case out of
        [sel] -> sendMessage (Chdir sel)
        _ -> pure ()

toggleFullscreen :: X ()
toggleFullscreen =
    withWindowSet $ \ws ->
    withFocused $ \w -> do
        let fullRect = W.RationalRect 0 0 1 1
        let isFullFloat = w `M.lookup` W.floating ws == Just fullRect
        windows $ if isFullFloat then W.sink w else W.float w fullRect

-- | Focus (or swap) n-th window group (SubLayouts). If already active, focus
-- the next window in the group.
focusNth :: (LayoutClass l Window, InspectLayout GetGroupStack l Window)
         => l Window -> Int -> Bool -> P.PureX Any
focusNth myLayout n swap = P.withFocii $ \_ focused -> do
    focusFrom focused <> if swap then swapWith focused else mempty
  where
    focusFrom focused = do
        ws <- P.curWorkspace
        case drop n (W.integrate' . getGroupStack myLayout $ ws) of
            [] -> mempty -- index out of bounds
            (g : _) | W.focus g /= focused -> P.focusWindow (W.focus g)
                    | otherwise -> P.focusWindow (W.focus . W.focusDown' $ g)

    swapWith _ | not swap = mempty
    swapWith oldFocused = P.withFocii $ \_ newFocused -> do
        P.putStack . fmap (swapStack oldFocused newFocused) =<< P.getStack
        P.focusWindow newFocused <> pure (Any $ oldFocused /= newFocused)

    swapStack a b (W.Stack f u d) = W.Stack (sw f) (map sw u) (map sw d)
      where sw x | x == a = b
                 | x == b = a
                 | otherwise = x

-- | Focus n-th physical screen.
focusNthScreen :: PhysicalScreen -> Bool -> X ()
focusNthScreen n greedy = do
    ws <- maybe mempty screenWorkspace =<< getScreen def n
    whenJust ws $ P.defile . (if greedy then P.greedyView else P.view)
