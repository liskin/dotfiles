{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Mainxmobar (main) where

import System.Directory
import XMonad.Util.My
import Xmobar

main = xmobar =<< configFromArgs =<< myConfig

myConfig = do
    hasEth0 <- doesPathExist "/sys/class/net/eth0"
    hasBat1 <- doesPathExist "/sys/class/power_supply/BAT1"
    let netCmds =
            [ Run $ Network "wlan1" ["-L","0","-H","32768","--normal","green","--high","red","-t","<fn=3>\xfaa8 </fn><rx>KB|<tx>KB "] 10 ] <>
            [ Run $ Network "eth0" ["-L","0","-H","32768","--normal","green","--high","red","-t","<fn=3>\xf6ff </fn><rx>KB:<tx>KB "] 10 | hasEth0 ]
    let batCmds =
            [ Run $ BatteryN ["BAT0"] ["-L","5","-H","70","--high","green","--low","red","-d","1","-t","<acstatus><left>%|<watts>W","--","-O","<icon=power-ac.xbm/>","-i","<icon=power-ac.xbm/>","-o","<icon=power-bat.xbm/>"] 50 "BAT0" ] <>
            [ Run $ BatteryN ["BAT1"] ["-L","5","-H","70","--high","green","--low","red","-d","1","-t","|<left>%|<watts>W"] 50 "BAT1" | hasBat1 ]
    pure baseConfig
        { template = concat $
            [ "%UnsafeXMonadLog% " ] <>
            [ "\a\a" ] <>
            [ "%_XMOBAR_DND%" ] <>
            [ "%_XMOBAR_DEVICES%" ] <>
            [ " " ] <>
            [ "%BAT0%" ] <>
            [ "%BAT1%" | hasBat1 ] <>
            [ "%_XMOBAR_BATTERY_EXTRA%" ] <>
            [ " " ] <>
            [ "%coretemp%" ] <>
            [ " " ] <>
            [ "%multicpu%" ] <>
            [ " " ] <>
            [ "%eth0%" | hasEth0 ] <>
            [ "%wlan1%" ] <>
            [ "<fc=white>%theDate%</fc>" ]
        , commands =
            [ Run $ Date "<fn=3>\xf5ef </fn>%a <action=`date +%F | xclip -r`>%F</action> %H:%M:%S" "theDate" 10
            , Run $ UnsafeXMonadLog
            , Run $ XPropertyLog "_XMOBAR_BATTERY_EXTRA"
            , Run $ XPropertyLog "_XMOBAR_DND"
            , Run $ XPropertyLog "_XMOBAR_DEVICES"
            , Run $ CoreTemp ["-t","<fn=3>\xf2c9 </fn><core0>C"] 50
            , Run $ MultiCpu ["-H","95","--high","red","-t","<icon=cpu.xbm/><icon=l.xbm/><autovbar><icon=r.xbm/>","--","--contiguous-icons"] 10
            ] <> netCmds <> batCmds
        }

baseConfig = defaultConfig
    { font = fontNormal
    , additionalFonts = [ fontBold, fontOblique, fontNerd, fontAweFree, fontAweFreeS, fontAweBrand ]
    , bgColor = "#200000"
    , fgColor = "#CFCFCF"
    , position = Top
    , iconRoot = myHome ++ "/.xmobar/icons"
    , alignSep = "\a\a"
    }
