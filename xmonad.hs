import XMonad
import System.IO
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS

-- Solarized color codes
base03   = "#002b36"
base02   = "#073642"
base01   = "#586e75"
base00   = "#657b83"
base0	   = "#839496"
base1	   = "#93a1a1"
base2	   = "#eee8d5"
base3	   = "#fdf6e3"
yellow   = "#b58900"
orange   = "#cb4b16"
red	   = "#dc322f"
magenta  = "#d33682"
violet   = "#6c71c4"
blue	   = "#268bd2"
cyan	   = "#2aa198"
green	   = "#859900"

myTerminal    = "urxvtc" -- use as a daemon
myModMask     = mod4Mask -- Win key or Super_L
myNormalBorderColor  = base3
myFocusedBorderColor = cyan
myBorderWidth        = 2

gapwidth = 5
gwU      = 5
gwD      = 5
gwL      = 5
gwR      = 5
myLayout = spacing gapwidth $ gaps [(U, gwU), (D, gwD), (L, gwL), (R, gwR)] $ Tall 1 0.01 0.5 ||| Tall 2 0.01 0.5 ||| Full
myKeysP = [
           ("M-p"      , spawn "rofi -show run")
          ,("<Print>"  , spawn "scrot ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png")
          ,("M-<Print>", spawn "scrot --focused ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png")
          ,("M-h"      , moveTo Prev NonEmptyWS)
          ,("M-l"      , moveTo Next NonEmptyWS)
          ,("M-n"      , moveTo Next EmptyWS)
          ,("M-<Left>" , sendMessage Shrink)
          ,("M-<Right>", sendMessage Expand)
          ]

myBar = "xmobar"

myXmobarPP = xmobarPP
   {
    ppCurrent         = xmobarColor yellow "" . wrap "[" "]"
   ,ppHidden          = xmobarColor cyan   "" . wrap " " " "
   ,ppHiddenNoWindows = xmobarColor base01 "" . wrap " " " "
   ,ppLayout          = xmobarColor base1  ""
   ,ppTitle           = xmobarColor yellow "" . shorten 60
   ,ppSep             = " | "
   ,ppWsSep           = ""
   }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main :: IO ()

main = do
   xmonad =<< statusBar myBar myXmobarPP toggleStrutsKey myConfig

myConfig = defaultConfig
   {
    terminal           = myTerminal
   ,modMask            = myModMask
   ,normalBorderColor  = myNormalBorderColor
   ,focusedBorderColor = myFocusedBorderColor
   ,borderWidth        = myBorderWidth
   ,layoutHook         = myLayout
   } `additionalKeysP` myKeysP
