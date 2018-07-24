import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Spacing

main = xmonad desktopConfig
   {
    terminal    = myTerminal
   ,modMask     = myModMask
   ,borderWidth = myBorderWidth
   ,layoutHook  = mySpacing
   }

myTerminal    = "urxvtc" -- use as a daemon
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 1
mySpacing     = smartSpacing 2 $ Tall 1 (3/100) (1/2) ||| Full
