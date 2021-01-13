{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-orphans -Wno-unused-top-binds #-}

module Main (main) where

import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

import Codec.Binary.UTF8.String (encodeString)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_, mapConcurrently_)
import Control.Exception (handle, SomeException)
import Control.Monad
import Control.Monad.Fix
import Data.List (intercalate, isPrefixOf, nub)
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Directory ( getCurrentDirectory )
import System.Environment
import System.Exit
import System.IO.Unsafe
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types

import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers hiding (pid)
import XMonad.Hooks.Place (placeHook, fixed)
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook, refocusLastWhen, isFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest(Simplest(..))
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed (addTabs, Shrinker(..), CustomShrink(..), Theme(..))
import XMonad.Layout.TrackFloating
import XMonad.Layout.WorkspaceDir
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Ungrab
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.PureX as P

import Xmobar.X11.Actions (stripActions)


up = updatePointer (0.5, 0.5) (0, 0)

cmdLogJournal =
    [ "systemd-cat"
    , "--priority=info", "--stderr-priority=warning", "--level-prefix=false"
    , "--" ]

cmdAppScope =
    [ "systemd-run", "--quiet", "--collect"
    , "--user" , "--scope", "--slice=app.slice", "--unit=\"app-$$.scope\""
    , "--" ]

cmdXCwd = [ "D=\"$(xcwd)\" || D=;", "${D:+cd \"$D\"};" ]

spawnExec s = spawn . unwords $ ["exec"] ++ cmdLogJournal ++ [s]
spawnApp s = spawn . unwords $ ["exec"] ++ cmdLogJournal ++ cmdAppScope ++ [s]
spawnTerm s = spawn . unwords $ cmdXCwd ++ ["exec"] ++ cmdLogJournal ++ cmdAppScope ++ [s]
spawnJournalPid s = spawnPID . unwords $ ["exec"] ++ cmdLogJournal ++ [s]

-- Bindings.
myKeys conf@(XConfig{modMask}) = M.fromList $
    -- running apps
    [ ((mod1Mask .|. controlMask, xK_r  ), unGrab >> spawnTerm "urxvt")
    , ((0,                     xK_Menu  ), unGrab >> spawnApp "rofi -show run")
    , ((controlMask,           xK_Menu  ), unGrab >> spawnApp "rofi -show drun")

    -- various rofi tools
    , ((modMask,               xK_Menu  ), unGrab >> spawnExec "rofi -show window")
    , ((modMask,               xK_e     ), unGrab >> spawnExec "rofi-emoji-menu")
    , ((modMask .|. shiftMask, xK_e     ), unGrab >> spawnExec "rofi-emoji-sign")
    , ((modMask,               xK_p     ), unGrab >> spawnExec "passmenu --type")

    -- media keys
    , ((0,         xF86XK_AudioMicMute    ), spawnExec "liskin-media mic-mute")
    , ((0,         xF86XK_AudioMute       ), spawnExec "liskin-media mute")
    , ((0,         xF86XK_AudioLowerVolume), spawnExec "liskin-media volume down")
    , ((0,         xF86XK_AudioRaiseVolume), spawnExec "liskin-media volume up")
    , ((0,         xF86XK_AudioPlay       ), spawnExec "liskin-media play")
    , ((0,         xF86XK_AudioPause      ), spawnExec "liskin-media play")
    , ((0,         xF86XK_AudioStop       ), spawnExec "liskin-media stop")
    , ((0,         xF86XK_AudioNext       ), spawnExec "liskin-media next")
    , ((0,         xF86XK_AudioPrev       ), spawnExec "liskin-media prev")

    -- other special keys
    , ((mod1Mask,  xK_space             ), spawnExec "liskin-touchpad-toggle")
    , ((0,         xF86XK_TouchpadToggle), spawnExec "liskin-touchpad-toggle")
    , ((0,         xF86XK_WebCam        ), spawnExec "liskin-touchscreen-toggle")
    , ((0,         xF86XK_Display       ), spawnExec "layout-auto layout-vertical")
    , ((0,         xF86XK_Sleep         ), spawnExec "layout-normal")
    , ((0,         xF86XK_Tools         ), spawn "sleep 0.5; xset dpms force off")
    , ((modMask,   xK_Print             ), unGrab >> spawnExec "scrot -u")
    , ((shiftMask, xK_Print             ), unGrab >> spawnExec "scrot -s")
    , ((modMask .|. shiftMask, xK_Print ), unGrab >> spawnExec "scrot")
    , ((0,         xF86XK_ScreenSaver   ), unGrab >> spawnExec "loginctl lock-session \"$XDG_SESSION_ID\"")
    , ((modMask,   xK_semicolon         ), unGrab >> spawnExec "loginctl lock-session \"$XDG_SESSION_ID\"")

    , ((modMask,               xK_Escape), kill)
    , ((modMask,               xK_space ), runSelectedAction "layout" laySels)
    , ((modMask .|. shiftMask, xK_space ), setLayout (XMonad.layoutHook conf) >> setCurrentWorkspaceName "")

    , ((modMask,               xK_j     ), windows W.focusDown   >> up)
    , ((modMask,               xK_k     ), windows W.focusUp     >> up)
    , ((modMask,               xK_Return), windows W.swapMaster  >> up)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown    >> up)
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp      >> up)

    , ((modMask,               xK_h     ), sendMessage Shrink            >> up)
    , ((modMask,               xK_l     ), sendMessage Expand            >> up)
    , ((modMask,               xK_u     ), sendMessage MirrorShrink      >> up)
    , ((modMask,               xK_i     ), sendMessage MirrorExpand      >> up)
    , ((modMask,               xK_m     ), sendMessage (Toggle REFLECTX) >> up)

    , ((modMask,               xK_w     ), toggleFullscreen)
    , ((modMask,               xK_t     ), withFocused (windows . W.sink) >> up)
    , ((modMask,               xK_f     ), toggleFloatNext >> runLogHook)
    , ((modMask .|. shiftMask, xK_f     ), toggleFloatAllNew >> runLogHook)

    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)    >> up)
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)) >> up)

    , ((modMask .|. shiftMask, xK_comma ), withFocused (sendMessage . mergeDir id) >> up)
    , ((modMask .|. shiftMask, xK_period), withFocused (sendMessage . UnMerge)     >> up)

    , ((modMask              , xK_x     ), sendMessage (ToggleStrut D))
    , ((modMask              , xK_z     ), sendMessage ToggleStruts)

    , ((modMask,               xK_c     ), changeDir def >> curDirToWorkspacename)
    , ((modMask,               xK_v     ), renameWorkspace def)
    , ((modMask,               xK_g     ), changeDirRofiGit >> curDirToWorkspacename)

    , ((modMask .|. shiftMask               , xK_q), rescreenHook)
    , ((modMask                             , xK_q), restart (myHome ++ "/bin/xmonad") True)
    , ((modMask .|. mod1Mask .|. controlMask, xK_q), io (exitWith ExitSuccess))
    ]
    ++

    [((m, k), P.defile f >> up)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F12]
        , (f, m) <- [(P.view i, mod1Mask), (P.shift i <> P.view i, controlMask)]]
    ++
    [((m, k), P.defile f >> up)
        | (i, k) <- zip (drop 12 $ XMonad.workspaces conf) [xK_F1 .. xK_F12]
        , (f, m) <- [(P.view i, modMask), (P.shift i <> P.view i, modMask .|. controlMask)]]
    ++
    [ ((modMask,               xK_n     ), toggleWS    >> up)
    , ((modMask,               xK_Left  ), prevWS      >> up)
    , ((modMask,               xK_Right ), nextWS      >> up)
    , ((modMask .|. shiftMask, xK_Left  ), swapTo Prev >> up)
    , ((modMask .|. shiftMask, xK_Right ), swapTo Next >> up)
    ]
    ++
    [ ((modMask .|. m, k), P.defile (focusNth i swap) >> up)
        | (i, k) <- zip [0..9] ([xK_1 .. xK_9] ++ [xK_0])
        , (m, swap) <- [(0, False), (shiftMask, True)] ]
    ++
    [ ((modMask,               xK_Tab   ), nextScreen >> up)
    , ((modMask .|. shiftMask, xK_Tab   ), prevScreen >> up) ]
    ++
    [ ((modMask .|. m, k), do { Just sc <- getScreen def psc; Just w <- screenWorkspace sc; P.defile (f w); up })
    | (k, psc) <- zip [xK_a, xK_s, xK_d] [0..]
    , (f, m) <- [(P.view, 0), (P.greedyView, shiftMask)] ]

myMouseBindings (XConfig{modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modMask, button2), windows . (W.swapMaster .) . W.focusWindow)
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

curDirToWorkspacename = do
    name <- getCurrentWorkspaceName
    when (isNothing name) $ do
        dir <- io getCurrentDirectory
        when (dir /= myHome) $ do
            setCurrentWorkspaceName $ last $ splitOneOf "/" dir

runSelectedAction :: String -> [(String, X ())] -> X ()
runSelectedAction prompt actions = do
    unGrab
    out <- lines <$> runProcessWithInput "rofi" ["-dmenu", "-p", prompt] (unlines $ map fst actions)
    case out of
        [sel] -> maybe (pure ()) id (sel `lookup` actions)
        _ -> pure ()

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

focusNth :: Int -> Bool -> P.PureX Any
focusNth n swap = P.withFocii $ \_ focused -> do
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


-- Layouts.
myLayout = dir . refocusLastLayoutHook . trackFloating $
    named "tiled" (fixl $ sub $ tiled) |||
    named "mtiled" (fixl $ sub $ Mirror $ tiled) |||
    named "tab" (fixl Full) |||
    named "grid" (fixl $ sub $ GridRatio (4/3)) |||
    named "spiral" (fixl $ sub $ spiral 0.618) |||
    named "full" (layoutHints $ noBorders Full)
  where
     tiled = ResizableTall 1 0.03 0.5 []
     dir = workspaceDir myHome
     toggles = mkToggle (single REFLECTX)
     fixl = avoidStruts . layoutHintsWithPlacement (0.5, 0.5) . smartBorders . toggles

     sub = addTabs CustomShrink decoTheme . subLayout [] Simplest
     decoTheme = def{
         decoHeight = 6,
         activeColor = "#ff0000", inactiveColor = "#dddddd", urgentColor = "#ffff00",
         activeBorderWidth = 1, inactiveBorderWidth = 1, urgentBorderWidth = 1,
         activeBorderColor = "#000000", inactiveBorderColor = "#000000", urgentBorderColor = "#ff0000"
     }

laySels = [ (s, sendMessage $ JumpToLayout s) | s <- l ]
    where l = [ "tiled"
              , "mtiled"
              , "tab"
              , "grid"
              , "spiral"
              , "full" ]

myHome = unsafePerformIO $ getEnv "HOME"

shortenDir :: String -> String
shortenDir s | (myHome ++ "/") `isPrefixOf` s = '~' : drop (length myHome) s
             | myHome == s                    = "~"
             | otherwise                      = s

instance Shrinker CustomShrink where
    shrinkIt _ _ = []


-- Managehook.
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , placeHook (fixed (0.5, 0.5))
    , floatNextHook
    , className =? "hl_linux" --> doFloat
    , className =? "duke3d" --> doFloat
    , className =? "cantata" --> doFloat
    , "_NET_WM_WINDOW_TYPE" `isInProperty` "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE" --> doIgnore <> doRaise
    , isDialog --> doFloat
    , transience'
    , manageDocks
    ]

myActivateHook = composeOne
    [ className =? "Google-chrome" <||> className =? "google-chrome" -?> doAskUrgent
    , pure True -?> doFocus
    ]

doLower, doRaise :: ManageHook
doLower = ask >>= \w -> liftX $ withDisplay $ \dpy -> io (lowerWindow dpy w) >> mempty
doRaise = ask >>= \w -> liftX $ withDisplay $ \dpy -> io (raiseWindow dpy w) >> mempty


-- Loghook.
myLogHook = do
    let myPP = xmobarPP
            { ppExtras =
                [ willFloatNextPP ("Float " ++)
                , willFloatAllNewPP ("Float " ++)
                , urgentsExtras
                ]
            , ppVisible = xmobarBorder "Top" "green" 1 . ppVisibleC
            , ppCurrent = xmobarBorder "Top" "yellow" 1 . ppCurrentC
            , ppUrgent = xmobarBorder "Top" "#ff0000" 1 . ppUrgentC
            , ppSep = " │ "
            , ppOrder = \(w:_:_:s) -> w:s
            }
    workspaceNamesPP myPP >>= dynamicLogString >>= xmonadPropLog
    xmobarWindowLists
    where
        ppVisibleC = xmobarColor "green" ""
        ppCurrentC = xmobarColor "yellow" ""
        ppNormalC = xmobarColor "#cfcfcf" ""
        ppUrgentC = xmobarColor "#ffff00" "#800000"
        shortenUrgent t | isWeechatTitle t = stripActions t
                        | otherwise = xmobarRaw $ shorten' "~" 30 t
        ppUrgentExtra urgents w = do
            nw <- getName w
            let pp = if w `elem` urgents then ppUrgentC else ppNormalC
            pure $ pp . shortenUrgent $ show nw
        urgentsExtras = do
            weechat <- weechatWins
            urgents <- readUrgents
            let ws = nub $ weechat ++ urgents
            if null ws
                then pure Nothing
                else do
                    items <- mapM (ppUrgentExtra urgents) ws
                    return $ Just (intercalate " " items)
        weechatWins :: X [Window]
        weechatWins = do
            ws <- gets windowset
            filterM isWeechat
                [ w | wks <- W.workspaces ws, W.tag wks == "1"
                , w <- W.integrate' (W.stack wks) ]
        isWeechat w = (isWeechatTitle . show) `fmap` getName w

isWeechatTitle = ("t[N] " `isPrefixOf`)


randrRestartEventHook (ConfigureEvent {ev_window = w}) = do
    r <- asks theRoot
    if w == r
        then do
            clearTypedWindowEvents w configureNotify
            rescreen
            rescreenHook
            return $ All False
        else
            return $ All True
randrRestartEventHook _ = mempty

trayerDockEventHook ConfigureEvent{ev_window = w, ev_above = a} | a == none = do
    -- when trayer is lowered to the bottom of stack, put all xmobars that
    -- are above it below
    whenX (runQuery (className =? "trayer") w) $ do
        withDisplay $ \dpy -> do
            rootw <- asks theRoot
            (_, _, ws) <- io $ queryTree dpy rootw
            let aboveTrayerWs = dropWhile (w /=) ws
            xmobarWs <- filterM (runQuery (appName =? "xmobar")) aboveTrayerWs
            mapM_ (io . lowerWindow dpy) xmobarWs
    mempty
trayerDockEventHook _ = mempty

myEventHook = mconcat
    [ refocusLastEventHook
    , hintsEventHook
    , randrRestartEventHook
    , trayerDockEventHook
    ]
    where
        refocusLastEventHook = refocusLastWhen isFloat

-- | clearEvents.  Remove all events of a given type from the event queue.
clearTypedWindowEvents :: Window -> EventType -> X ()
clearTypedWindowEvents w t = withDisplay $ \d -> io $ do
    sync d False
    allocaXEvent $ \p -> fix $ \again -> do
        more <- checkTypedWindowEvent d w t p
        when more again -- beautiful

rescreenHook :: X ()
rescreenHook = do
    let mainxmobar = sequence [ spawnJournalPid "xmobar -x 0" ]
    let trayer = sequence [ spawnJournalPid "trayer --align right --height 17 --widthtype request --alpha 255 --transparent true --monitor primary -l" ]
    killPids
    savePids . concat =<< sequence [ xmobarScreens, mainxmobar, trayer ]
    spawnExec "~/bin/.xlayout/post.sh"

xmobarScreens :: X [ ProcessID ]
xmobarScreens = do
    ws <- gets windowset
    forM (W.screens ws) $ \scr -> do
        let S num = W.screen scr
            n = show num
            prop = "_XMONAD_LOG_SCREEN_" ++ n
        spawnJournalPid $ "xmobar -b -x " ++ n ++ " -c '[Run XPropertyLog \"" ++ prop ++ "\"]' -t '%" ++ prop ++ "%'"

xmobarWindowLists :: X ()
xmobarWindowLists = withWindowSet $ \ws -> do
    addWksName <- getWorkspaceNames
    urgents <- readUrgents
    forM_ (W.screens ws) $ \scr -> do
        let wks = W.workspace scr
        let tag = W.tag wks
        let stack = W.stack wks
        let isFocused = (W.focus <$> stack ==) . Just
        let isCurrent = tag == W.tag (W.workspace (W.current ws))

        let tagFmt | isCurrent = ppCurrentC
                   | otherwise = ppVisibleC

        let dir' = fromMaybe "<err>" $ getWorkspaceDir myLayout wks
        let dir = shortenLeft 30 . shortenDir $ dir'
        let layout = description . W.layout $ wks
        let logHeader = unwords [tagFmt (addWksName tag), dir, layout]

        let winFmt w | isFocused w && isCurrent = ppFocusC
            winFmt w | w `elem` urgents         = ppUrgentC
                     | otherwise                = ppUnfocusC

        let wins = W.integrate' stack
        tits <- mapM getName wins
        let gs = map W.integrate . W.integrate' . getGroupStack myLayout $ wks
        let indices = [ i | (n, g) <- zip [1..] gs
                      , i <- primes [ show (n :: Int) | _ <- g ] ]
        let logWins = [ " │ " ++ winFmt w (i ++ " " ++ sanitize (show tit))
                      | w <- wins | tit <- tits | i <- indices ]

        xmobarLog scr . concat $ logHeader : logWins

    where
        ppVisibleC = xmobarBorder "Bottom" "green" 1 . xmobarColor "green" ""
        ppCurrentC = xmobarBorder "Bottom" "yellow" 1 . xmobarColor "yellow" ""

        ppFocusC   = xmobarBorder "Bottom" "#ffff00" 1 . xmobarColor "#ffff00" ""
        ppUrgentC  = xmobarColor "#ff0000" "#ffff00"
        ppUnfocusC = xmobarColor "#b0b040" ""

        sanitize t = xmobarRaw . shorten' "~" 30 . strip $ t
          where
            strip | isWeechatTitle t = xmobarStrip
                  | otherwise        = id

        xmobarLog (W.screen -> S n) = xmonadPropLog' prop . encodeString
          where prop = "_XMONAD_LOG_SCREEN_" ++ show n

        primes [n] = [n]
        primes ns = [ n ++ [p] | n <- ns | p <- "⠁⠃⠇⡇⡏⡟⡿⣿" ++ ['a'..] ]

data KillPids = KillPids [ ProcessID ] deriving (Show, Read)

instance ExtensionClass KillPids where
    initialValue = KillPids []
    extensionType = PersistentExtension

savePids :: [ ProcessID ] -> X ()
savePids = XS.put . KillPids

killPids :: X ()
killPids = do
    KillPids pids <- XS.get
    io $ mapConcurrently_ killPid pids

killPid :: ProcessID -> IO ()
killPid pid = try_ $ race_ (killer 50000) waiter
    where
        try_ = handle (\(_ :: SomeException) -> pure ())
        waiter = getProcessStatus True False pid
        killer delay = do
            signalProcess (if delay < 1000000 then sigTERM else sigKILL) pid
            threadDelay delay
            killer (delay * 2)


-- Startuphook.
myStartupHook = do
    rescreenHook

javaHack cfg = cfg { startupHook = startupHook cfg >> setWMName "LG3D" }

-- Main.
main = do
    xmonad $
        javaHack .
        docks .
        ewmh' def
            { activateHook = myActivateHook
            , workspaceListTransform = workspaceNamesListTransform
            , fullscreen = True
            } .
        withUrgencyHookC NoUrgencyHook urgencyConfig{ suppressWhen = Focused } $
            def
            { terminal           = "urxvt"
            , focusFollowsMouse  = True
            , borderWidth        = 2
            , modMask            = mod4Mask
            , workspaces         = map show [(1 :: Int)..12] ++ map (('W' :) . show) [(1 :: Int)..12]
            , normalBorderColor  = "#dddddd"
            , focusedBorderColor = "#ff0000"

            , keys               = myKeys
            , mouseBindings      = myMouseBindings

            , layoutHook         = myLayout
            , manageHook         = myManageHook
            , logHook            = myLogHook
            , startupHook        = myStartupHook
            , handleEventHook    = myEventHook
            }
