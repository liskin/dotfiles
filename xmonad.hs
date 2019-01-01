{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

import Control.Applicative
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
import System.Exit
import System.IO.Unsafe
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types

import XMonad.Actions.CycleWS
import XMonad.Actions.FocusNth
import XMonad.Actions.GridSelect
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spiral
import XMonad.Layout.TrackFloating
import XMonad.Layout.WorkspaceDir
import XMonad.Util.NamedWindows
import XMonad.Util.SpawnOnce
import XMonad.Util.Stack


up = updatePointer (0.5, 0.5) (0, 0)

xF86XK_TouchpadToggle = 269025193
xF86XK_AudioMicMute = 0x1008FFB2

-- Bindings.
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((mod1Mask .|. controlMask, xK_r  ), spawn $ "exec " ++ XMonad.terminal conf)
    , ((modMask,            xK_semicolon), spawn "sleep 0.1; xscreensaver-command -lock")
    , ((0,            xF86XK_ScreenSaver), spawn "sleep 0.1; xscreensaver-command -lock")
    , ((0,                     xK_Menu  ), spawn "exec dmenu_run -f -fn Fixed-10")
    , ((modMask,               xK_Menu  ), goToSelected def >> up)
    , ((modMask,               xK_grave ), goToSelected def >> up)
    , ((modMask,               xK_c     ), changeDir def >> curDirToWorkspacename)
    , ((modMask,               xK_v     ), renameWorkspace def)
    , ((modMask,               xK_g     ), sendMessage (Chdir (myHome ++ "/work/GoodData")))

    , ((0,         xF86XK_AudioMicMute  ), spawn "pactl set-source-mute alsa_input.pci-0000_00_1f.3.analog-stereo toggle")
    , ((0,         xF86XK_TouchpadToggle), spawn "touchpad_toggle")
    , ((0,         xF86XK_WebCam        ), spawn "touchscreen_toggle")
    , ((0,         xF86XK_Display       ), spawn "layout-auto")
    , ((0,         xF86XK_Sleep         ), spawn "layout-normal")
    , ((mod1Mask,      xK_space         ), spawn "touchpad_toggle")

    , ((modMask,               xK_Escape), kill)
    , ((modMask .|. controlMask, xK_space ), sendMessage NextLayout)
    , ((modMask,               xK_space ), runSelectedAction def laySels)
    , ((modMask .|. shiftMask, xK_space ), setLayout (XMonad.layoutHook conf) >> setCurrentWorkspaceName "")
    , ((controlMask,           xK_space ), refresh)

    , ((mod1Mask,              xK_Tab   ), windows W.focusDown   >> up)
    , ((modMask,               xK_j     ), windows W.focusDown   >> up)
    , ((modMask,               xK_k     ), windows W.focusUp     >> up)
    , ((modMask,               xK_m     ), windows W.focusMaster >> up)
    , ((modMask,               xK_Return), windows W.swapMaster  >> up)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown    >> up)
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp      >> up)

    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask,               xK_u     ), sendMessage ShrinkSlave)
    , ((modMask,               xK_i     ), sendMessage ExpandSlave)

    , ((modMask,               xK_w     ), withFocused $ \w -> windows $ W.float w (W.RationalRect 0 0 1 1))
    , ((modMask,               xK_t     ), withFocused (windows . W.sink) >> up)
    , ((modMask,               xK_f     ), toggleFloatNext >> runLogHook)
    , ((modMask .|. shiftMask, xK_f     ), toggleFloatAllNew >> runLogHook)

    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    , ((modMask              , xK_x     ), sendMessage (ToggleStrut D))
    , ((modMask              , xK_z     ), sendMessage ToggleStruts)

    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart (myHome ++ "/bin/xmonad") True)
    , ((mod1Mask .|. controlMask, xK_q  ), dumpLayouts)
    ]
    ++

    [((m, k), windows f >> up)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F12]
        , (f, m) <- [(W.view i, mod1Mask), (W.view i . W.shift i, controlMask)]]
    ++
    [((m, k), windows f >> up)
        | (i, k) <- zip (drop 12 $ XMonad.workspaces conf) [xK_F1 .. xK_F12]
        , (f, m) <- [(W.view i, modMask), (W.view i . W.shift i, modMask .|. controlMask)]]
    ++
    [ ((modMask,               xK_n     ), toggleWS    >> up)
    , ((modMask,               xK_Left  ), prevWS      >> up)
    , ((modMask,               xK_Right ), nextWS      >> up)
    , ((modMask .|. shiftMask, xK_Left  ), swapTo Prev >> up)
    , ((modMask .|. shiftMask, xK_Right ), swapTo Next >> up)
    ]
    ++
    [ ((modMask .|. m, k), focusNth i >> a >> up) | (i, k) <- zip [0..9] ([xK_1 .. xK_9] ++ [xK_0]),
        (m, a) <- [ (0, return ()), (shiftMask, windows W.swapMaster) ] ]
    ++
    [ ((modMask,               xK_Tab   ), nextScreen >> up)
    , ((modMask .|. shiftMask, xK_Tab   ), prevScreen >> up) ]
    ++
    [ ((modMask .|. m, k), do { Just sc <- getScreen psc; Just w <- screenWorkspace sc; windows (f w); up })
    | (k, psc) <- zip [xK_a, xK_s, xK_d] [0..]
    , (f, m) <- [(W.view, 0), (W.greedyView, shiftMask)] ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((mod1Mask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((mod1Mask, button2), windows . (W.swapMaster .) . W.focusWindow)
    , ((mod1Mask, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

curDirToWorkspacename = do
    name <- getCurrentWorkspaceName
    when (isNothing name) $ do
        dir <- io getCurrentDirectory
        setCurrentWorkspaceName $ last $ splitOneOf "/" dir


-- Layouts.
myLayout = {-flexibleRead $ -} dir $
    named "tiled" (fixl mrt) |||
    named "mtiled" (fixl mrt') |||
    named "tab" (fixl Full) |||
    named "grid" (fixl (GridRatio $ 4/3)) |||
    named "spiral" (fixl (spiral $ 0.618)) |||
    named "full" (layoutHints $ noBorders Full)
  where
     mrt = mouseResizableTile          { draggerType = BordersDragger }
     mrt' = mouseResizableTileMirrored { draggerType = BordersDragger }
     dir = workspaceDir myHome
     fixl x  = trackFloating . avoidStruts . layoutHintsWithPlacement (0.5, 0.5) . smartBorders $ x
     -- layoutHints _musi_ byt pred (po :-)) smartBorders, jinak blbne urxvt

laySels = [ (s, sendMessage $ JumpToLayout s) | s <- l ]
    where l = [ "tiled"
              , "mtiled"
              , "tab"
              , "grid"
              , "spiral"
              --, "float"
              , "full" ]

-- $( flexibleReadInstance 'myLayout )

myHome = unsafePerformIO $ getEnv "HOME"


-- Managehook.
myManageHook = composeAll
    [ floatNextHook
    -- , className =? "MPlayer"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "wmix"           --> doHideIgnore
    , className =? "hl_linux"       --> doFloat
    , className =? "duke3d"         --> doFloat
    , isFullscreen                  --> doFullFloat
    , isDialog                      --> doFloat
    , transience'
    , manageDocks
    ]
    -- > xprop | grep WM_CLASS


-- Loghook.
myLogHook = do
    let myPP = xmobarPP
            { ppExtras =
                [ willFloatNextPP ("Float " ++)
                , willFloatAllNewPP ("Float " ++)
                , urgentsExtras
                ]
            , ppVisible = xmobarColor "green" "" . ppVisible xmobarPP
            , ppUrgent = ppUrgentC
            , ppSep = " | "
            , ppOrder = \(w:_:_:s) -> w:s
            }
    workspaceNamesPP myPP >>= dynamicLogString >>= xmonadPropLog
    xmobarWindowLists
    --fadeInactiveLogHook 0.87
    where
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

myEventHook = hintsEventHook <+> myEvHook

-- | clearEvents.  Remove all events of a given type from the event queue.
clearTypedWindowEvents :: Window -> EventType -> X ()
clearTypedWindowEvents w t = withDisplay $ \d -> io $ do
    sync d False
    allocaXEvent $ \p -> fix $ \again -> do
        more <- checkTypedWindowEvent d w t p
        when more again -- beautiful

rescreenHook :: X ()
rescreenHook = do
    disp <- io $ getEnv "DISPLAY"
    let mainxmobar = sequence [ spawnPID "exec xmobar -x 0" ]
    let trayer = sequence [ spawnPID "exec trayer --align right --height 17 --widthtype request --alpha 255 --transparent true --monitor primary" ]
    let compton = sequence [ spawnPID "exec compton" ]
    killPids "_XMONAD_XMOBARS"
    savePids "_XMONAD_XMOBARS" . concat =<< sequence [ xmobarScreens, mainxmobar, trayer, compton ]
    spawn "exec fbsetroot -mod 5 5 -fg rgb:00/10/00 -bg rgb:00/00/00"

xmobarScreens :: X [ ProcessID ]
xmobarScreens = do
    ws <- gets windowset
    forM (W.screens ws) $ \scr -> do
        let S num = W.screen scr
            n = show num
            prop = "_XMONAD_LOG_SCREEN_" ++ show num
        spawnPID $ "exec xmobar -b -x " ++ n ++ " -c '[Run XPropertyLog \"_XMONAD_LOG_SCREEN_" ++ n ++ "\"]' -t '%_XMONAD_LOG_SCREEN_" ++ n ++ "%'"

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
                | (b,w,t) <- wins | n <- [1..] ]
        dynamicLogString finPP >>= xmonadPropLog' prop

    where
        myPP l = xmobarPP
            { ppSep = "   "
            , ppOrder = \(_:_:_:l) -> l
            , ppExtras = map (return . Just) l
            , ppVisible = xmobarColor "green" "" . ppVisible xmobarPP
            }

screenWins scr = forM stack $ either (name False) (name True)
    where
        stack = toTags . W.stack . W.workspace $ scr
        name b = \w -> (,,) <$> pure b <*> pure w <*> getName w

savePids :: String -> [ ProcessID ] -> X ()
savePids prop pids = do
    d <- asks display
    r <- asks theRoot
    prop <- getAtom prop
    typ <- getAtom "PID"
    io $ changeProperty32 d r prop typ propModeReplace $ map fromIntegral pids

getPids :: String -> X [ ProcessID ]
getPids prop = do
    d <- asks display
    r <- asks theRoot
    prop <- getAtom prop
    fmap (map fromIntegral . fromMaybe []) $ io $ getWindowProperty32 d prop r

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
    disp <- io $ getEnv "DISPLAY"
    mapM_ spawnOnce
        [ "xset r rate 200 25"
        , "xset s off"
        , "xset b off"
        , "xset dpms 300 300 300"
        , "xsetroot -cursor_name left_ptr"
        , "xmodmap ~/.Xmodmap"
        , "exec redshift"
        , "xprop -root -remove _NET_WORKAREA"
        , "exec pa-applet"
        ]
    rescreenHook
    when (disp == ":0") $ mapM_ spawnOnce
        [ "exec /usr/lib/notify-osd/notify-osd"
        , "exec nm-applet"
        , "exec blueman-applet"
        , "start-pulseaudio-x11"
        , "exec /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1"
        , "exec firewall-applet"
        , "exec caffeine-indicator"
        ]

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
            modMask            = mod4Mask,
            workspaces         = map show [1..12] ++ map (('W' :) . show) [1..12],
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
