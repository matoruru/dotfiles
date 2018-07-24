import XMonad

main = xmonad $ defaultConfig {
      terminal    = myTerminal
     ,modMask     = myModMask
     ,borderWidth = myBorderWidth
    }

myTerminal    = "urxvtc" -- use as a daemon
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 1
