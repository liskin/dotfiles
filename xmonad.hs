{-# LANGUAGE FlexibleInstances, TemplateHaskell, ParallelListComp #-}
import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Monoid
import Data.Tree
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama
import System.Directory ( getCurrentDirectory )
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Posix.Process

import XMonad.Actions.CycleWS
import XMonad.Actions.FocusNth
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationMadness
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Util.Run
import XMonad.Util.NamedWindows
import XMonad.Util.SpawnOnce

import XMonad.Layout.FlexibleRead


-- Defaults.
myTerminal      = "urxvt"
myBorderWidth   = 2
myModMask       = mod4Mask
myWorkspaces    = map show [1..12]
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"


-- Bindings.
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((mod1Mask .|. controlMask, xK_r  ), spawn $ XMonad.terminal conf)
    , ((mod1Mask .|. controlMask, xK_t  ), spawn "LANG=cs_CZ rxvt")
    , ((mod1Mask .|. controlMask, xK_h  ), spawn "LANG=cs_CZ rxvt -e /home/tomi/bin/hnb")
    , ((modMask,            xK_semicolon), spawn "xlock")
    , ((0,                     xK_Menu  ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modMask,               xK_Menu  ), goToSelected defaultGSConfig)
    , ((modMask,               xK_c     ), changeDir defaultXPConfig)
    , ((modMask,               xK_v     ), renameWorkspace defaultXPConfig)

    , ((0, xF86XK_AudioLowerVolume), spawn "killall -USR2 wmix")
    , ((0, xF86XK_AudioRaiseVolume), spawn "killall -USR1 wmix")
    , ((0, xF86XK_AudioMute), spawn "if amixer get Master | grep \"\\[off\\]\" ; then amixer set Master on; else amixer set Master off; fi")

    , ((modMask,               xK_Escape), kill)
    , ((modMask .|. controlMask, xK_space ), sendMessage NextLayout)
    , ((modMask,               xK_space ), runSelectedAction defaultGSConfig laySels)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((controlMask,           xK_space ), refresh)

    , ((mod1Mask,              xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp  )
    , ((modMask,               xK_m     ), windows W.focusMaster  )
    , ((modMask,               xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)

    , ((modMask,               xK_w     ), withFocused $ \w -> windows $ W.float w (W.RationalRect 0 0 1 1))
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    , ((modMask,               xK_f     ), toggleFloatNext >> runLogHook)
    , ((modMask .|. shiftMask, xK_f     ), toggleFloatAllNew >> runLogHook)

    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    , ((modMask              , xK_s     ), sendMessage ToggleStruts)

    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart "xmonad" True)
    , ((mod1Mask .|. controlMask, xK_q  ), dumpLayouts)
    ]
    ++

    [((m, k), windows f)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F12]
        , (f, m) <- [(W.view i, mod1Mask), (W.view i . W.shift i, controlMask)]] ++
    [ ((modMask,               xK_n     ), toggleWS)
    , ((modMask .|. shiftMask, xK_Left  ), swapTo Prev)
    , ((modMask .|. shiftMask, xK_Right ), swapTo Next)
    ]
    ++
    [ ((modMask .|. m, k), focusNth i >> a) | (i, k) <- zip [0..9] ([xK_1 .. xK_9] ++ [xK_0]),
        (m, a) <- [ (0, return ()), (shiftMask, windows W.swapMaster) ] ]
    -- [ ((modMask, k), focusNth i) | (i, k) <- zip [0..9] ([xK_1 .. xK_9] ++ [xK_0]) ]
    ++

    [ ((modMask,               xK_Tab   ), nextScreen)
    , ((modMask .|. shiftMask, xK_Tab   ), swapNextScreen) ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((mod1Mask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((mod1Mask, button2), (\w -> focus w >> {-windows W.swapMaster >>-} windows W.shiftMaster))
    , ((mod1Mask, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]


-- Layouts.
myLayout = {-flexibleRead $ -} dir $
    named "tiled" (fixl mouseResizableTile) |||
    named "mtiled" (fixl mouseResizableTileMirrored) |||
    named "tab" (fixl $ simpleTabbed) |||
    named "float" floating |||
    named "full" (layoutHints $ noBorders Full)
  where
     dir = workspaceDir "~"
     fixl x  = avoidStruts . layoutHintsWithPlacement (0.5, 0.5) . smartBorders $ x
     -- layoutHints _musi_ byt pred (po :-)) smartBorders, jinak blbne urxvt
     floating = avoidStruts . decoration shrinkText decoTheme (Simple False) .
         mouseResize . layoutHints . smartBorders . windowArrangeAll $ SF 20
     decoTheme = defaultTheme { decoWidth = 2000 }

laySels = [ (s, sendMessage $ JumpToLayout s) | s <- l ]
    where l = [ "tiled", "mtiled", "tab", "float", "full" ]

-- $( flexibleReadInstance 'myLayout )


-- Managehook.
myManageHook = composeAll
    [ floatNextHook
    , className =? "MPlayer"        --> doFloat
    --, className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "wmix"           --> doHideIgnore
    , isFullscreen                  --> doFullFloat
    , isDialog                      --> doFloat
    , transience'
    , manageDocks
    ]
    -- > xprop | grep WM_CLASS


-- Mousefocus.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True


-- Loghook.
myLogHook = do
    namesPP <- workspaceNamesPP xmobarPP
    let myPP = namesPP
            { ppExtras =
                [ (Just . shortenL 30 . shortenDir) `fmap` io getCurrentDirectory
                , willFloatNextPP ("Float " ++)
                , willFloatAllNewPP ("Float " ++)
                ]
            , ppVisible = xmobarColor "green" "" . ppVisible namesPP
            , ppSep = " | "
            }
    dynamicLogString myPP >>= xmonadPropLog
    xmobarWindowLists
    updatePointer (Relative 0.5 0.5)


-- Current directory printer.
myHome = unsafePerformIO $ getEnv "HOME"

shortenDir :: String -> String
shortenDir s = case myHome `isPrefixOf` s of
    False -> s
    True -> '~' : drop (length myHome) s

shortenL :: Int -> String -> String
shortenL n xs | length xs < n = xs
              | otherwise     = end ++ (take (n - length end) xs)
 where
    end = "..."


-- Restart xmobar on RAndR.
myEventHook (ConfigureEvent {ev_window = w}) = do
    whenX (isRoot w) restartxmobar
    return $ All True
myEventHook _ = mempty

restartxmobar :: X ()
restartxmobar = do
    disp <- io $ getEnv "DISPLAY"
    when (disp /= ":1") $ do
        pid <- spawnPID "killall xmobar"
        io $ catch (getProcessStatus True False pid) (const $ return undefined)
        spawn "xmobar -x 1"
        xmobarScreens

xmobarScreens :: X ()
xmobarScreens = do
    ws <- gets windowset
    forM_ (W.screens ws) $ \scr -> do
        let S num = W.screen scr
            n = show num
            prop = "_XMONAD_LOG_SCREEN_" ++ show num
        spawn $ "xmobar -b -x " ++ n ++ " -c '[Run XPropertyLog \"_XMONAD_LOG_SCREEN_" ++ n ++ "\"]' -t '%_XMONAD_LOG_SCREEN_" ++ n ++ "%' -f '-misc-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*'"

xmobarWindowLists :: X ()
xmobarWindowLists = do
    ws <- gets windowset
    let S current = W.screen $ W.current ws
    forM_ (W.screens ws) $ \scr -> do
        (l,c,r) <- screenWins scr
        let S num = W.screen scr
            prop = "_XMONAD_LOG_SCREEN_" ++ show num

            active = xmobarColor "#ffff00" "" . shorten 30
            inactive = xmobarColor "#808000" "" . shorten 30

            act = if current == num then active else inactive
            l1 = zip (repeat inactive) l ++ zip (repeat act) c ++ zip (repeat inactive) r
            finPP = myPP [ f (show n ++ " " ++ show t) | (f,t) <- l1 | n <- [1..] ]
        dynamicLogString finPP >>= xmonadPropLog' prop

    where
        myPP l = xmobarPP
            { ppSep = "  "
            , ppOrder = \(_:_:_:l) -> l
            , ppExtras = map (return . Just) l
            }

screenWins scr = case W.stack . W.workspace $ scr of
    Nothing -> return ([],[],[])
    Just (W.Stack x l r) -> do
        l' <- mapM getName (reverse l)
        x' <- getName x
        r' <- mapM getName r
        return (l', [x'], r')


-- Startuphook.
myStartupHook = do
    setWMName "LG3D"
    disp <- io $ getEnv "DISPLAY"
    when (disp /= ":1") $ mapM_ spawnOnce
        [ "xset r rate 200 25"
        , "xset s off"
        , "xset dpms 300 300 300"
        , "bsetroot -mod 5 5 -fg rgb:00/10/00 -bg rgb:00/00/00"
        , "killall halevt; halevt"
        , "kwalletd"
        , "wmix"
        , "pkill -f '^udprcv 12200'; udprcv 12200 | xmonadpropwrite _XMONAD_LOG_IRSSI"
        ]
    restartxmobar

dumpLayouts = do
    lay <- gets windowset
    io $ appendFile "/tmp/xmonad_layout_log" ("\n\n-----\n" ++ show lay)

-- Main.
main = do
    -- putStr $ drawTree $ fmap show $ (read $ show myLayout :: Tree (String, String))

    -- threadDelay 100000
    let defaults = defaultConfig {
            terminal           = myTerminal,
            focusFollowsMouse  = myFocusFollowsMouse,
            borderWidth        = myBorderWidth,
            modMask            = myModMask,
            workspaces         = myWorkspaces,
            normalBorderColor  = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,

            keys               = myKeys,
            mouseBindings      = myMouseBindings,

            layoutHook         = myLayout,
            manageHook         = myManageHook,
            logHook            = myLogHook,
            startupHook        = myStartupHook,
            handleEventHook    = myEventHook
            }
    xmonad $
        withUrgencyHook NoUrgencyHook $
        defaults
