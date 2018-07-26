import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps

myTerminal    = "urxvtc" -- use as a daemon
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 0

gapwidth = 6 
gwU      = 24 -- bacause height of dmenu is 18.
gwD      = 6
gwL      = 6
gwR      = 6
mySpacing   = spacing gapwidth $ gaps [(U, gwU), (D, gwD), (L, gwL), (R, gwR)] $ Tall 1 0.05 0.5 ||| Full
main = xmonad defaultConfig
   {
    terminal    = myTerminal
   ,modMask     = myModMask
   ,borderWidth = myBorderWidth
   ,layoutHook  = mySpacing
   }

