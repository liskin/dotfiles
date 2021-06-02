import XMonad
import XMonad.Config.Bluetile

main :: IO ()
main = xmonad bluetileConfig{ terminal = "urxvt" }
