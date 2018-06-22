import XMonad

main = do
  xmonad $ defaultConfig
    { terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = myBorderWidth
    }

myTerminal    = "urxvtc"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 1
