{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Environment
import System.Exit
import "regex-compat-tdfa" Text.Regex
import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.MessageFeedback
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers hiding (pid)
import XMonad.Hooks.Place (placeHook, fixed)
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook, refocusLastWhen, isFloat)
import XMonad.Hooks.Rescreen
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
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
import XMonad.Prompt
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Hacks
import XMonad.Util.NamedWindows
import XMonad.Util.Ungrab
import qualified XMonad.Util.PureX as P

import XMonad.Actions.DoNotDisturb
import XMonad.Hooks.WriteState
import XMonad.Util.My

import Xmobar.X11.Actions (stripActions)

-- Bindings
myKeys conf@(XConfig{modMask}) = M.fromList $
    -- running apps
    [ ((altMask .|. controlMask, xK_r   ), unGrab >> spawnTerm "urxvt")
    , ((0,                     xK_Menu  ), unGrab >> spawnApp "rofi -show run")
    , ((controlMask,           xK_Menu  ), unGrab >> spawnApp "rofi -show drun")

    -- various rofi tools
    , ((modMask,               xK_Menu  ), unGrab >> spawnExec "rofi -show window")
    , ((modMask,               xK_e     ), unGrab >> spawnExec "rofi-emoji-menu")
    , ((modMask .|. shiftMask, xK_e     ), unGrab >> spawnExec "rofi-emoji-sign")
    , ((modMask,               xK_p     ), unGrab >> spawnExec "passmenu")
    , ((modMask,               xK_o     ), unGrab >> spawnExec "dunstctl context")

    -- media keys
    , ((0,       xF86XK_AudioMicMute    ), spawnExec "liskin-media mic-mute")
    , ((0,       xF86XK_AudioMute       ), spawnExec "liskin-media mute")
    , ((0,       xF86XK_AudioLowerVolume), spawnExec "liskin-media volume down")
    , ((0,       xF86XK_AudioRaiseVolume), spawnExec "liskin-media volume up")
    , ((0,       xF86XK_AudioPlay       ), spawnExec "liskin-media play")
    , ((0,       xF86XK_AudioPause      ), spawnExec "liskin-media play")
    , ((0,       xF86XK_AudioStop       ), spawnExec "liskin-media stop")
    , ((0,       xF86XK_AudioNext       ), spawnExec "liskin-media next")
    , ((0,       xF86XK_AudioPrev       ), spawnExec "liskin-media prev")

    -- other special keys
    , ((altMask,   xK_space             ), spawnExec "liskin-touchpad-toggle")
    , ((0,         xF86XK_TouchpadToggle), spawnExec "liskin-touchpad-toggle")
    , ((0,         xF86XK_WebCam        ), spawnExec "liskin-touchscreen-toggle")
    , ((0,         xF86XK_Display       ), spawnExec "layout-auto layout-vertical")
    , ((0,         xF86XK_Sleep         ), spawnExec "layout-normal")
    , ((0,         xF86XK_Tools         ), spawn "sleep 0.5; xset dpms force off")
    , ((0,         xK_Print             ), unGrab >> spawnExec "flameshot gui")
    , ((modMask,   xK_Print             ), unGrab >> spawnExec "scrot -u")
    , ((shiftMask, xK_Print             ), unGrab >> spawnExec "scrot -s")
    , ((modMask .|. shiftMask, xK_Print ), unGrab >> spawnExec "scrot")
    , ((0,         xF86XK_ScreenSaver   ), unGrab >> spawnExec "loginctl lock-session \"$XDG_SESSION_ID\"")
    , ((modMask,   xK_semicolon         ), unGrab >> spawnExec "loginctl lock-session \"$XDG_SESSION_ID\"")

    -- focus changes
    , ((modMask,               xK_j     ), windows W.focusDown   >> up)
    , ((modMask,               xK_k     ), windows W.focusUp     >> up)
    , ((modMask,               xK_Return), windows W.swapMaster  >> up)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown    >> up)
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp      >> up)

    -- workspace/screen focus changes
    , ((modMask,               xK_n     ), toggleWS    >> up)
    , ((modMask,               xK_Left  ), prevWS      >> up)
    , ((modMask,               xK_Right ), nextWS      >> up)
    , ((modMask .|. shiftMask, xK_Left  ), swapTo Prev >> up)
    , ((modMask .|. shiftMask, xK_Right ), swapTo Next >> up)
    , ((modMask,               xK_Tab   ), nextScreen  >> up)
    , ((modMask .|. shiftMask, xK_Tab   ), prevScreen  >> up)

    -- window actions
    , ((modMask,               xK_Escape), kill)
    , ((modMask,               xK_w     ), toggleFullscreen)
    , ((modMask,               xK_t     ), withFocused (windows . W.sink) >> up)

    -- layout changes
    , ((modMask,               xK_space ), runSelectedAction "layout" laySels)
    , ((modMask .|. shiftMask, xK_space ), setLayout (XMonad.layoutHook conf) >> setCurrentWorkspaceName "")
    , ((modMask,               xK_h     ), sendMessage Shrink            >> up)
    , ((modMask,               xK_l     ), sendMessage Expand            >> up)
    , ((modMask .|. shiftMask, xK_h     ), sendMessages (replicate 5 Shrink) >> up)
    , ((modMask .|. shiftMask, xK_l     ), sendMessages (replicate 5 Expand) >> up)
    , ((modMask,               xK_u     ), sendMessage MirrorShrink      >> up)
    , ((modMask,               xK_i     ), sendMessage MirrorExpand      >> up)
    , ((modMask .|. shiftMask, xK_u     ), sendMessages (replicate 5 MirrorShrink) >> up)
    , ((modMask .|. shiftMask, xK_i     ), sendMessages (replicate 5 MirrorExpand) >> up)
    , ((modMask,               xK_m     ), sendMessage (Toggle REFLECTX) >> up)
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)    >> up)
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)) >> up)
    , ((modMask .|. shiftMask, xK_comma ), withFocused (sendMessage . mergeDir id) >> up)
    , ((modMask .|. shiftMask, xK_period), withFocused (sendMessage . UnMerge)     >> up)

    -- toggles
    , ((modMask,               xK_f     ), toggleFloatNext >> runLogHook)
    , ((modMask .|. shiftMask, xK_f     ), toggleFloatAllNew >> runLogHook)
    , ((modMask              , xK_x     ), sendMessage (ToggleStrut D))
    , ((modMask              , xK_z     ), sendMessage ToggleStruts)

    -- workspace dir and name
    , ((modMask,               xK_c     ), changeDir xpConfig  >> curDirToWorkspacename)
    , ((modMask,               xK_v     ), renameWorkspace xpConfig)
    , ((modMask,               xK_g     ), changeDirRofiGit >> curDirToWorkspacename)

    -- restart/rescreen
    , ((modMask              , xK_q     ), restart (myHome ++ "/bin/xmonad") True)

    -- the end
    , ((modMask .|. altMask .|. controlMask, xK_q), io (exitWith ExitSuccess))
    ] ++
    -- focus changes
    [ ((modMask .|. m, k), P.defile (focusNth myLayout i swap) >> up)
        | (i, k) <- zip [0..9] ([xK_1 .. xK_9] ++ [xK_0])
        , (m, swap) <- [(0, False), (shiftMask, True)]
    ] ++
    -- workspace/screen focus changes
    [ ((m, k), P.defile f >> up)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F12]
        , (f, m) <- [(P.view i, altMask), (P.shift i <> P.view i, controlMask)]
    ] ++
    [ ((m, k), P.defile f >> up)
        | (i, k) <- zip (drop 12 $ XMonad.workspaces conf) [xK_F1 .. xK_F12]
        , (f, m) <- [(P.view i, modMask), (P.shift i <> P.view i, modMask .|. controlMask)]
    ] ++
    [ ((modMask .|. m, k), focusNthScreen i greedy >> up)
        | (i, k) <- zip [0..] [xK_a, xK_s, xK_d]
        , (m, greedy) <- [(0, False), (shiftMask, True)]
    ]
  where
    altMask = mod1Mask

myMouseBindings (XConfig{modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modMask, button2), windows . (W.swapMaster .) . W.focusWindow)
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

up :: X ()
up = updatePointer (0.5, 0.5) (0, 0)

xpConfig :: XPConfig
xpConfig = def
    { font = "-misc-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"
    , bgColor = "#200000"
    , fgColor = "#CFCFCF"
    , height = 22
    , position = CenteredAt 0.5 0.5
    , promptBorderWidth = 2
    , showCompletionOnTab = True
    }

-- Layouts
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
         decoHeight = 4,
         activeColor = "#ff0000", inactiveColor = "#dddddd", urgentColor = "#ffff00",
         activeBorderWidth = 0, inactiveBorderWidth = 1, urgentBorderWidth = 1,
         activeBorderColor = "#000000", inactiveBorderColor = "#000000", urgentBorderColor = "#ff0000"
     }

laySels :: [(String, X ())]
laySels = [ (s, jumpToLayout s) | s <- l ]
    where l = [ "tiled"
              , "mtiled"
              , "tab"
              , "grid"
              , "spiral"
              , "full" ]

instance Shrinker CustomShrink where
    shrinkIt _ _ = []

-- Manage hook
myManageHook :: ManageHook
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , placeHook (fixed (0.5, 0.5))
    , floatNextHook
    , className =? "hl_linux" --> doFloat
    , className =? "duke3d" --> doFloat
    , className =? "cantata" --> doFloat
    , appName =? "alarm-clock-applet" --> doFloat
    , appName =? "blueman-manager" --> doFloat
    , appName =? "pavucontrol" --> doFloat
    , "_NET_WM_WINDOW_TYPE" `isInProperty` "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE" --> doIgnore <> doRaise
    , isDialog --> doFloat
    , transience'
    ]

myFloatConfReqManageHook :: MaybeManageHook
myFloatConfReqManageHook = composeAll
    [ appName =? "alarm-clock-applet" -?> doFloat -- prevent alarm-clock-applet from moving its floats to remebered location
    , className =? "Steam" -?> doFloat -- prevent Steam from moving its floats to primary screen
    ]

myActivateHook :: ManageHook
myActivateHook = composeOne
    [ className =? "Google-chrome" <||> className =? "google-chrome" -?> doAskUrgent
    , pure True -?> doFocus
    ]

-- Event hook
myEventHook :: Event -> X All
myEventHook = mconcat
    [ refocusLastEventHook
    , hintsEventHook
    , trayerAboveXmobarEventHook
    , floatConfReqHook myFloatConfReqManageHook
    , serverModeEventHookF "_XMONAD_CTL" (mconcat myXmonadCtlHooks)
    ]
    where
        refocusLastEventHook = refocusLastWhen isFloat
        myXmonadCtlHooks = [myCtlDnd]

floatConfReqHook :: MaybeManageHook -> Event -> X All
floatConfReqHook mh ConfigureRequestEvent{ev_window = w} = do
    runQuery (join <$> (isFloat -?> mh)) w >>= \case
        Nothing -> mempty
        Just e -> windows (appEndo e) >> pure (All False)
floatConfReqHook _ _ = mempty

-- Do Not Disturb
myCtlDnd :: String -> X ()
myCtlDnd "dnd-on" = do
    setDoNotDisturb DoNotDisturb
    withUrgents $ mapM_ myUrgencyHook
    jumpToLayout' "1" "tab"
    focusWiki
myCtlDnd "dnd-off" = do
    setDoNotDisturb Disturb
    replayDeferredUrgents
    jumpToLayout' "1" "tiled"
myCtlDnd _ = mempty

myUrgencyHook :: Window -> X ()
myUrgencyHook = deferUrgencyHook $ isDND <&&> windowTag =? Just "1"

focusWiki :: X ()
focusWiki = withWorkspace "1" $ focusQueryWin $ do
    t <- title
    pure $ " - VIM" `isSuffixOf` t && "~/taskwiki" `isInfixOf` t

-- Status bars, tray(er)
myStatusBars :: ScreenId -> StatusBarConfig
myStatusBars 0 = xmobarTop <> xmobarBottom 0 <> trayerBottom
myStatusBars n = xmobarBottom n

xmobar :: String -> [String] -> X PP -> StatusBarConfig
xmobar prop args pp = xmobar' prop args (dynamicLogString =<< pp)

xmobar' :: String -> [String] -> X String -> StatusBarConfig
xmobar' prop args pp = statusBarGeneric cmd (xmonadPropLog' prop =<< pp)
  where
    cmd = cmdExecJournal $ unwords $ ["xmobar", "-i", myHome ++ "/.xmobar/icons"] ++ args

xmobarTop :: StatusBarConfig
xmobarTop = xmobar xmonadDefProp ["-x", "0"] $ do
    dnd <- getDoNotDisturb
    clickablePP . workspaceIconsPP =<< workspaceNamesPP . boldPP =<< pure xmobarPP
        { ppExtras =
            [ urgentsExtras dnd
            , willFloatNextPP ("Float " ++)
            , willFloatAllNewPP ("Float " ++)
            ]
        , ppVisible = ppVisibleB . ppVisibleC
        , ppCurrent = ppCurrentB . ppCurrentC
        , ppUrgent = ppUrgentB . ppUrgentC
        , ppSep = " │ "
        , ppOrder = \(w:_:_:s) -> w:s
        }
  where
    boldPP pp = pp{ ppRename = ppRename pp . fnBold }
    workspaceIconsPP pp = pp{ ppRename = ppRename pp >=> pure . workspaceIcons }
    ppVisibleC = xmobarColor "green" ""
    ppVisibleB = xmobarBorder "Top" "green" 1
    ppCurrentC = xmobarColor "yellow" ""
    ppCurrentB = xmobarBorder "Top" "yellow" 1
    ppUrgentC = xmobarColor "#ffff00" "#800000:3,1"
    ppUrgentB = xmobarBorder "Top" "#ff0000" 1

    urgentsExtras DoNotDisturb = mempty
    urgentsExtras Disturb = do
        weechatWs <- weechatWins
        urgentsWs <- readUrgents
        weechat <- mapM (ppUrgentExtra id) weechatWs
        urgents <- mapM (ppUrgentExtra ppUrgentC) (urgentsWs \\ weechatWs)
        pure $ unwordsExtras $ weechat ++ urgents

    ppUrgentExtra pp w = ppClickableW w . pp . shortenUrgent . show <$> getName w

    weechatWins :: X [Window]
    weechatWins = do
        ws <- gets windowset
        filterM isWeechat
            [ w | wks <- W.workspaces ws, W.tag wks == "1"
            , w <- W.integrate' (W.stack wks) ]

    isWeechat w = (isWeechatTitle . show) `fmap` getName w

    unwordsExtras xs | null xs   = Nothing
                     | otherwise = Just (intercalate " │ " xs)

xmobarBottom :: ScreenId -> StatusBarConfig
xmobarBottom sid@(S (show -> sn)) = xmobar' prop args $ withScreen $ \scr -> do
    currentTag <- withWindowSet $ pure . W.currentTag
    addWksName <- getWorkspaceNames ":"
    urgents <- readUrgents

    let wks = W.workspace scr
    let tag = W.tag wks
    let stack = W.stack wks
    let isFocused = (W.focus <$> stack ==) . Just
    let isCurrent = tag == currentTag

    let tagFmt | isCurrent = ppCurrentC
               | otherwise = ppVisibleC

    let tag' = tagFmt $ workspaceIcons $ addWksName (fnBold tag) wks
    let dir' = fromMaybe "<err>" $ getWorkspaceDir myLayout wks
    let dir = shortenLeft 30 . shortenDir $ dir'
    let layout = "<icon=layout-" ++ description (W.layout wks) ++ ".xbm/>"
    let logHeader = unwords [tag', layout, dir]

    let winFmt w | isFocused w && isCurrent = ppFocusC
                 | w `elem` urgents         = ppUrgentC
                 | otherwise                = ppUnfocusC
    let clickWinFmt w = ppClickableW w . winFmt w

    let wins = W.integrate' stack
    tits <- mapM getName wins
    let gs = map W.integrate . W.integrate' . getGroupStack myLayout $ wks
    let indices = [ i | (n, g) <- zip [1..] gs
                  , i <- primes [ fnBold (show (n :: Int)) | _ <- g ] ]
    let logWins = [ " │ " ++ clickWinFmt w (i ++ " " ++ sanitize (show tit))
                  | w <- wins | tit <- tits | i <- indices ]

    pure . concat $ logHeader : logWins
  where
    prop = "_XMONAD_LOG_SCREEN_" ++ sn
    args = [ "-c", "'[Run UnsafeXPropertyLog \"" ++ prop ++ "\"]'"
           , "-t", "'%" ++ prop ++ "%'", "-b", "-x", sn]

    withScreen f = do
        s <- gets (find ((sid ==) . W.screen) . W.screens . windowset)
        P.whenJust' s f

    ppVisibleC = xmobarBorder "Bottom" "green" 1 . xmobarColor "green" ""
    ppCurrentC = xmobarBorder "Bottom" "yellow" 1 . xmobarColor "yellow" ""

    ppFocusC   = xmobarBorder "Bottom" "#ffff00" 1 . xmobarColor "#ffff00" ""
    ppUrgentC  = xmobarColor "#ffff00" "#800000:3,1"
    ppUnfocusC = xmobarColor "#b0b040" ""

    sanitize t = xmobarRaw . shorten' "~" 30 . strip $ t
      where
        strip | isWeechatTitle t = xmobarStrip
              | otherwise        = id

    primes [n] = [n]
    primes ns = [ n ++ [p] | n <- ns | p <- "⠁⠃⠇⡇⡏⡟⡿⣿" ++ ['a'..] ]

    shortenDir s | (myHome ++ "/") `isPrefixOf` s = '~' : drop (length myHome) s
                 | myHome == s                    = "~"
                 | otherwise                      = s

trayerBottom :: StatusBarConfig
trayerBottom = flip statusBarGeneric mempty $ cmdExecJournal $ unwords
    [ "trayer"
    , "--align", "right"
    , "--height", "17"
    , "--widthtype", "request"
    , "--alpha", "255"
    , "--transparent", "true"
    , "--monitor", "primary"
    , "-l"
    ]

isWeechatTitle :: String -> Bool
isWeechatTitle t = "t[N] weechat: " `isPrefixOf` t || "weechat/matrix: " `isPrefixOf` t

workspaceIcons :: String -> String
workspaceIcons = s "\\<irc\\>" (fnNerd "\xf198")
               . s "\\<web\\>" (fnNerd "\xfa9e")
               . s "\\<watch\\>" (fnAweBrand "\xf167") -- fnNerd "\xf947"
               . s "\\<steam\\>" (fnNerd "\xf1b7")
               . s "\\<xmonad\\>" ("X" ++ fnNerd "\xe61f")
               . s "\\<strava\\>" (fnAweBrand "\xf428")
               . s "\\<foursquare\\>" (fnAweBrand "\xf180")
               . s "\\<python\\>" (fnNerd "\xf81f")
  where
    s re sub x = subRegex (mkRegex re) x sub

shortenUrgent :: String -> String
shortenUrgent t
    | Just x <- stripPrefix "t[N] weechat: " t = fnNerd "\xf198" ++ ":" ++ stripActions x
    | Just x <- stripPrefix "weechat/matrix: " t = "M:" ++ stripActions x
    | Just x <- stripPrefix "t[m] m[N] " t = fnAweFree "\xf0e0" ++ " " ++ s x
    | otherwise = s t
  where
    s = xmobarRaw . shorten' "~" 30

-- Startup hook
myStartupHook :: X ()
myStartupHook = do
    primary <- ((Just "1") ==) <$> io (lookupEnv "_LISKIN_SESSION_PRIMARY")
    first <- isNothing <$> getWorkspaceName "1"
    when (primary && first) $ do
        mapM_ (uncurry setWorkspaceName) [("1", "irc"), ("2", "web"), ("12", "watch")]

-- Main.
main = do
    xmonad $
        writeStateHook $
        addAfterRescreenHook (spawnExec "~/bin/.xlayout/post.sh") $
        addRandrChangeHook (spawnExec "if-session-unlocked layout-auto") $
        dynamicSBs (pure . myStatusBars) $
        docks $
        ewmh' def
            { activateHook = myActivateHook
            , workspaceRename = workspaceNamesRenameWS
            , fullscreen = True
            } $
        withUrgencyHookC myUrgencyHook urgencyConfig{ suppressWhen = Focused } $
        def { terminal           = "urxvt"
            , focusFollowsMouse  = True
            , borderWidth        = 2
            , modMask            = mod4Mask
            , workspaces         = map show [(1 :: Int)..12] ++ map (('W' :) . show) [(1 :: Int)..12]
            , normalBorderColor  = "#dddddd"
            , focusedBorderColor = "#ff0000"

            , keys               = myKeys
            , mouseBindings      = myMouseBindings

            , startupHook        = myStartupHook
            , layoutHook         = myLayout
            , manageHook         = myManageHook
            , handleEventHook    = myEventHook
            }
