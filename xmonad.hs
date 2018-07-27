import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig

myTerminal    = "urxvtc" -- use as a daemon
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 0

gapwidth = 6 
gwU      = 6
gwD      = 6
gwL      = 6
gwR      = 6
mySpacing   = spacing gapwidth $ gaps [(U, gwU), (D, gwD), (L, gwL), (R, gwR)] $ Tall 1 0.05 0.5 ||| Tall 2 0.05 0.5 ||| Full
myKeysP = [ ("M-p", spawn "rofi -show run") ]

main = xmonad =<< xmobar ( defaultConfig
   {
    terminal    = myTerminal
   ,modMask     = myModMask
   ,borderWidth = myBorderWidth
   ,layoutHook  = mySpacing
   } `additionalKeysP` myKeysP )
