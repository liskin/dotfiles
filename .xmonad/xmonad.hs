{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -fno-warn-orphans #-}
import XMonad hiding ((|||), modMask)
import qualified XMonad
import qualified XMonad.StackSet as W

import Control.Exception ( try, SomeException )
import Control.Monad
import Control.Monad.Fix
import Data.List ( intercalate, isPrefixOf, nub )
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Directory ( getCurrentDirectory )
import System.Environment
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
import XMonad.Util.Stack
import XMonad.Util.Ungrab
import qualified XMonad.Util.PureX as P


up = updatePointer (0.5, 0.5) (0, 0)

-- Bindings.
myKeys conf@(XConfig {XMonad.modMask}) = M.fromList $
    [ ((mod1Mask .|. controlMask, xK_r  ), unGrab >> spawn ("exec " ++ XMonad.terminal conf))
    , ((modMask,            xK_semicolon), unGrab >> spawn "loginctl lock-session")
    , ((0,            xF86XK_ScreenSaver), unGrab >> spawn "loginctl lock-session")
    , ((0,                     xK_Menu  ), unGrab >> spawn "exec rofi -show run")
    , ((controlMask,           xK_Menu  ), unGrab >> spawn "exec rofi -show drun")
    , ((modMask,               xK_Menu  ), unGrab >> spawn "exec rofi -show window")
    , ((modMask,               xK_e     ), unGrab >> spawn "exec emoji-menu.sh")
    , ((modMask,               xK_p     ), unGrab >> spawn "passmenu --type")

    , ((0,         xF86XK_AudioMicMute    ), spawn "liskin-media mic-mute")
    , ((0,         xF86XK_AudioMute       ), spawn "liskin-media mute")
    , ((0,         xF86XK_AudioLowerVolume), spawn "liskin-media volume down")
    , ((0,         xF86XK_AudioRaiseVolume), spawn "liskin-media volume up")
    , ((0,         xF86XK_AudioPlay       ), spawn "liskin-media play")
    , ((0,         xF86XK_AudioPause      ), spawn "liskin-media play")
    , ((0,         xF86XK_AudioStop       ), spawn "liskin-media stop")
    , ((0,         xF86XK_AudioNext       ), spawn "liskin-media next")
    , ((0,         xF86XK_AudioPrev       ), spawn "liskin-media prev")

    , ((0,         xF86XK_TouchpadToggle), spawn "liskin-touchpad-toggle")
    , ((0,         xF86XK_WebCam        ), spawn "liskin-touchscreen-toggle")
    , ((0,         xF86XK_Display       ), spawn "layout-auto layout-vertical")
    , ((0,         xF86XK_Sleep         ), spawn "layout-normal")
    , ((0,         xF86XK_Tools         ), spawn "sleep 0.5; xset dpms force off")
    , ((mod1Mask,      xK_space         ), spawn "liskin-touchpad-toggle")

    , ((modMask,               xK_Escape), kill)
    , ((modMask .|. controlMask, xK_space ), sendMessage NextLayout)
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

    , ((modMask .|. shiftMask, xK_q     ), rescreenHook)
    , ((modMask              , xK_q     ), restart (myHome ++ "/bin/xmonad") True)
    , ((mod1Mask .|. controlMask, xK_q  ), dumpLayouts)
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
    [ ((modMask .|. m, k), P.defile (P.focusNth i <> a) >> up)
        | (i, k) <- zip [0..9] ([xK_1 .. xK_9] ++ [xK_0])
        , (m, a) <- [ (0, return (Any False)), (shiftMask, P.modifyWindowSet' W.swapMaster >> return (Any True)) ] ]
    ++
    [ ((modMask,               xK_Tab   ), nextScreen >> up)
    , ((modMask .|. shiftMask, xK_Tab   ), prevScreen >> up) ]
    ++
    [ ((modMask .|. m, k), do { Just sc <- getScreen def psc; Just w <- screenWorkspace sc; P.defile (f w); up })
    | (k, psc) <- zip [xK_a, xK_s, xK_d] [0..]
    , (f, m) <- [(P.view, 0), (P.greedyView, shiftMask)] ]

myMouseBindings (XConfig {XMonad.modMask}) = M.fromList $
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
     -- layoutHints _musi_ byt pred (po :-)) smartBorders, jinak blbne urxvt

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

instance Shrinker CustomShrink where
    shrinkIt _ _ = []


-- Managehook.
myManageHook = composeAll
    [ placeHook (fixed (0.5, 0.5))
    , floatNextHook
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "hl_linux"       --> doFloat
    , className =? "duke3d"         --> doFloat
    , isFullscreen                  --> doFullFloat
    , isDialog                      --> doFloat
    , transience'
    , manageDocks
    ]


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
            , ppSep = " | "
            , ppOrder = \(w:_:_:s) -> w:s
            }
    workspaceNamesPP myPP >>= dynamicLogString >>= xmonadPropLog
    xmobarWindowLists
    --fadeInactiveLogHook 0.87
    where
        ppVisibleC = xmobarColor "green" ""
        ppCurrentC = xmobarColor "yellow" ""
        ppNormalC = xmobarColor "#cfcfcf" ""
        ppUrgentC = xmobarColor "#ffff00" "#800000"
        shortenUrgent t | isWeechatTitle t = t
                        | otherwise = shorten 30 (xmobarStrip t)
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
                , w <- fst (toIndex (W.stack wks)) ]
        isWeechat w = (isWeechatTitle . show) `fmap` getName w
        isWeechatTitle = ("t[N] " `isPrefixOf`)


-- Restart xmobar on RAndR.
myEvHook (ConfigureEvent {ev_window = w}) = do
    r <- asks theRoot
    if w == r
        then do
            clearTypedWindowEvents w configureNotify
            rescreen
            rescreenHook
            return $ All False
        else
            return $ All True
-- myEvHook x = io (print x) >> mempty
myEvHook _ = mempty

myEventHook = refocusLastEventHook <+> hintsEventHook <+> myEvHook
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
    let mainxmobar = sequence [ spawnPID "exec xmobar -x 0" ]
    let trayer = sequence [ spawnPID "exec trayer --align right --height 17 --widthtype request --alpha 255 --transparent true --monitor primary" ]
    let compton = sequence [ spawnPID "exec compton" ]
    killPids "_XMONAD_XMOBARS"
    savePids "_XMONAD_XMOBARS" . concat =<< sequence [ xmobarScreens, mainxmobar, trayer, compton ]
    spawn "exec ~/bin/.xlayout/post.sh"

xmobarScreens :: X [ ProcessID ]
xmobarScreens = do
    ws <- gets windowset
    forM (W.screens ws) $ \scr -> do
        let S num = W.screen scr
            n = show num
            prop = "_XMONAD_LOG_SCREEN_" ++ n
        spawnPID $ "exec xmobar -b -x " ++ n ++ " -c '[Run XPropertyLog \"" ++ prop ++ "\"]' -t '%" ++ prop ++ "%'"

xmobarWindowLists :: X ()
xmobarWindowLists = do
    name <- getWorkspaceNames
    ws <- gets windowset
    urgents <- readUrgents
    let S current = W.screen $ W.current ws
    forM_ (W.screens ws) $ \scr -> do
        wins <- screenWins scr
        let S num = W.screen scr
            prop = "_XMONAD_LOG_SCREEN_" ++ show num
            wks = W.workspace scr
            tag = W.tag wks
            layout = description . W.layout $ wks

            fmt (True, _, n) | num == current =
                     xmobarColor "#ffff00" ""        $ n
            fmt (_,    w, n) = if w `elem` urgents
                then xmobarColor "#ff0000" "#ffff00" $ n
                else xmobarColor "#808000" ""        $ n

            tagprint = if current == num
                then ppCurrent finPP
                else ppVisible finPP

            finPP = myPP $ (tagprint (name tag) ++ " " ++ layout) :
                [ fmt (b, w, show n ++ " " ++ shorten 30 (xmobarStrip (show t)))
                | (b,w,t) <- wins | n <- [(1 :: Int)..] ]
        dynamicLogString finPP >>= xmonadPropLog' prop

    where
        myPP l = xmobarPP
            { ppSep = "   "
            , ppOrder = \(_:_:_:x) -> x
            , ppExtras = map (return . Just) l
            , ppVisible = xmobarBorder "Bottom" "green" 1 . ppVisibleC
            , ppCurrent = xmobarBorder "Bottom" "yellow" 1 . ppCurrentC
            }
        ppVisibleC = xmobarColor "green" ""
        ppCurrentC = xmobarColor "yellow" ""

screenWins scr = forM stack $ either (name False) (name True)
    where
        stack = toTags . W.stack . W.workspace $ scr
        name b = \w -> (,,) <$> pure b <*> pure w <*> getName w

savePids :: String -> [ ProcessID ] -> X ()
savePids prop pids = do
    d <- asks display
    r <- asks theRoot
    aprop <- getAtom prop
    typ <- getAtom "PID"
    io $ changeProperty32 d r aprop typ propModeReplace $ map fromIntegral pids

getPids :: String -> X [ ProcessID ]
getPids prop = do
    d <- asks display
    r <- asks theRoot
    aprop <- getAtom prop
    fmap (map fromIntegral . fromMaybe []) $ io $ getWindowProperty32 d aprop r

killPids :: String -> X ()
killPids prop = do
    pids <- getPids prop
    io $ mapM_ killPid pids

killPid :: ProcessID -> IO ()
killPid pid = do
    _ :: Either SomeException () <- try $ do
        signalProcess sigTERM pid
        void $ getProcessStatus True False pid
    return ()


-- Startuphook.
myStartupHook = do
    rescreenHook

javaHack cfg = cfg { startupHook = startupHook cfg >> setWMName "LG3D" }

dumpLayouts = do
    lay <- gets windowset
    io $ appendFile "/tmp/xmonad_layout_log" ("\n\n-----\n" ++ show lay)

-- Main.
main = do
    -- putStr $ drawTree $ fmap show $ (read $ show myLayout :: Tree (String, String))

    let defaults = def {
            terminal           = "urxvt",
            focusFollowsMouse  = True,
            borderWidth        = 2,
            XMonad.modMask     = mod4Mask,
            workspaces         = map show [(1 :: Int)..12] ++ map (('W' :) . show) [(1 :: Int)..12],
            normalBorderColor  = "#dddddd",
            focusedBorderColor = "#ff0000",

            keys               = myKeys,
            mouseBindings      = myMouseBindings,

            layoutHook         = myLayout,
            manageHook         = myManageHook,
            logHook            = myLogHook,
            startupHook        = myStartupHook,
            handleEventHook    = myEventHook
            }
    let activationIgnore = className =? "Google-chrome" <||> className =? "google-chrome"
    xmonad $
        javaHack $
        ignoreNetActiveWindow activationIgnore $
        docks $
        ewmhFullscreen $
        ewmh $
        withUrgencyHookC NoUrgencyHook urgencyConfig{ suppressWhen = Focused } $
        defaults
