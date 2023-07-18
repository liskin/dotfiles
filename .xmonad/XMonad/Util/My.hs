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
import Data.List
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
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Inspect
import XMonad.Layout.SubLayouts
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Util.Run
import XMonad.Util.Ungrab
import XMonad.Util.WindowProperties
import qualified XMonad.Util.PureX as P

{-# NOINLINE myHome #-}
myHome :: String
myHome = unsafePerformIO $ getEnv "HOME"

jumpToLayout :: String -> X ()
jumpToLayout = sendMessage . JumpToLayout

jumpToLayout' :: WorkspaceId -> String -> X ()
jumpToLayout' t s = withWorkspace t $ \w ->
    P.handlingRefresh $ sendMessageWithNoRefresh (JumpToLayout s) w

-- | Update workspace name (if empty) to current directory
curDirToWorkspacename :: X ()
curDirToWorkspacename = do
    name <- getCurrentWorkspaceName
    when (isNothing name) $ do
        dir <- io getCurrentDirectory
        when (dir /= myHome) $ do
            setCurrentWorkspaceName $ last $ splitOneOf "/" dir

-- | Like 'renameWorkspace' but with the current name in the prompt.
renameWorkspace :: XPConfig -> X ()
renameWorkspace conf = do
    n <- getCurrentWorkspaceName
    XMonad.Actions.WorkspaceNames.renameWorkspace conf{ defaultText = fromMaybe "" n }

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

isFloat :: Window -> X Bool
isFloat w = gets $ M.member w . W.floating . windowset

isFloatQ :: Query Bool
isFloatQ = ask >>= liftX . isFloat

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

-- | Do something with a named workspace.
withWorkspace :: WorkspaceId -> (WindowSpace -> X ()) -> X ()
withWorkspace t f = do
    w <- gets $ find ((t ==) . W.tag) . W.workspaces . windowset
    whenJust w f

-- | Do something with windows selected by @Query Bool@.
withQueryWin :: Query Bool -> ([Window] -> X ()) -> WindowSpace -> X ()
withQueryWin q f ws = filterM (runQuery q) (W.integrate' (W.stack ws)) >>= f

-- | Focus first window selected by @Query Bool@.
focusQueryWin :: Query Bool -> WindowSpace -> X ()
focusQueryWin q ws = withQueryWin q f ws
    where
        f [] = mempty
        f (w:_) = windows $ onWorkspace (W.tag ws) (W.focusWindow w)
        onWorkspace n g s = W.view (W.currentTag s) . g . W.view n $ s

-- | Run 'Query' on currently focused window.
peekQ :: Query Bool -> Query Bool
peekQ q = getAny <$> peekQ' (Any <$> q)

peekQ' :: Monoid a => Query a -> Query a
peekQ' q = do
    w <- liftX $ gets $ W.peek . windowset
    maybe mempty (flip local q . const) w

fnBold, fnOblique, fnNerd, fnAweFree, fnAweFreeS, fnAweBrand :: String -> String
fnBold = wrap "<fn=1>" "</fn>"
fnOblique = wrap "<fn=2>" "</fn>"
fnNerd = wrap "<fn=3>" "</fn>" . concatMap (: " ")
fnAweFree = wrap "<fn=4>" "</fn>"
fnAweFreeS = wrap "<fn=5>" "</fn>"
fnAweBrand = wrap "<fn=6>" "</fn>"

fontNormal, fontBold, fontOblique, fontNerd, fontAweFree, fontAweFreeS, fontAweBrand :: String
fontNormal = "-misc-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"
fontBold = "-misc-fixed-bold-r-normal-*-13-*-*-*-*-*-*-*"
fontOblique = "-misc-fixed-medium-o-normal-*-13-*-*-*-*-*-*-*"
fontNerd = "xft:Inconsolata Nerd Font-11"
fontAweFree = "xft:Font Awesome 5 Free-12"
fontAweFreeS = "xft:Font Awesome 5 Free Solid-12"
fontAweBrand = "xft:Font Awesome 5 Brands-12"

ppClickableW :: Window -> String -> String
ppClickableW w = xmobarAction ("xdotool windowactivate " ++ show w) "1"

cmdLogJournal, cmdAppScope, cmdXCwd :: [String]
cmdLogJournal =
    [ "systemd-cat"
    , "--priority=info", "--stderr-priority=warning", "--level-prefix=false"
    , "--" ]
cmdAppScope =
    [ "systemd-run", "--quiet", "--collect"
    , "--user" , "--scope", "--slice=app.slice", "--unit=\"app-$$.scope\""
    , "--" ]
cmdXCwd = [ "D=\"$(xcwd)\" || D=;", "${D:+cd \"$D\"};" ]

focusedIsTerminal :: X Bool
focusedIsTerminal = focusedHasProperty (ClassName "URxvt")

spawnExec, spawnApp, spawnTerm :: String -> X ()
spawnExec s = spawn . unwords $ ["exec"] ++ cmdLogJournal ++ [s]
spawnApp s = spawn . unwords $ ["exec"] ++ cmdLogJournal ++ cmdAppScope ++ [s]
spawnTerm s = do
    cmdXCwd' <- P.whenM' focusedIsTerminal (pure cmdXCwd)
    spawn . unwords $ cmdXCwd' ++ ["exec"] ++ cmdLogJournal ++ cmdAppScope ++ [s]

cmdExecJournal :: String -> String
cmdExecJournal s = unwords $ ["exec"] ++ cmdLogJournal ++ [s]
