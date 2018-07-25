import XMonad
import XMonad.Layout.Spacing

main = xmonad defaultConfig
   {
    terminal    = myTerminal
   ,modMask     = myModMask
   ,borderWidth = myBorderWidth
   ,layoutHook  = mySpacing
   }

myTerminal    = "urxvtc" -- use as a daemon
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 0
mySpacing     = spacing 8 $ Tall 1 0.05 0.5 ||| Full -- Tall (num of master) (propotion of move by sizing) (width of master)
