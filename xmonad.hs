import XMonad
import System.IO
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W

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

darkgreen	   = "#748800"
lightgreen	   = "#96aa44"

myTerminal    = "urxvtc" -- use as a daemon
myModMask     = mod4Mask -- Win key or Super_L
myNormalBorderColor  = base0
myFocusedBorderColor = cyan
myBorderWidth        = 3

gapwidth = 4
gwU      = 4
gwD      = 4
gwL      = 4
gwR      = 4
myLayout = spacing gapwidth $ gaps [(U, gwU), (D, gwD), (L, gwL), (R, gwR)] $ Tall 1 0.01 0.5 ||| Tall 2 0.01 0.5 ||| Full
myKeysP = [
           ("M-p"         , spawn "rofi -show run")
          ,("M-u"         , spawn "urxvtc")
          ,("M-i"         , spawn "qutebrowser")
          ,(  "<Print>"   , spawn "scrot           ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png")
          ,("M-<Print>"   , spawn "scrot --focused ~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png")
          ,("M-h"         , moveTo   Prev NonEmptyWS)
          ,("M-l"         , moveTo   Next NonEmptyWS)
          ,("M-n"         , do
                              windows $ W.greedyView "9"
                              moveTo Next    EmptyWS)
          ,("M-<Left>"    , sendMessage Shrink)
          ,("M-<Right>"   , sendMessage Expand)
          ,("M-c"         , kill)
          ,("M-S-<Return>", spawn "")
          ,("M-<Tab>"     , spawn "")
          ,("M-S-<Tab>"   , spawn "")
          ,("M-1"         , spawn "")
          ,("M-2"         , spawn "")
          ,("M-3"         , spawn "")
          ,("M-4"         , spawn "")
          ,("M-5"         , spawn "")
          ,("M-6"         , spawn "")
          ,("M-7"         , spawn "")
          ,("M-8"         , spawn "")
          ,("M-9"         , spawn "")
          ]

myManageHook = composeAll
   [ className =? "~/Pictures/Screenshots/tmp/fehF.sh" --> doFloat ]

myBar = "xmobar"

myXmobarPP = xmobarPP
   {
    ppCurrent         = xmobarColor darkgreen "" . \s -> "[<fc=#96aa44>$</fc>]"
   ,ppHidden          = xmobarColor darkgreen "" . \s -> "[ ]"
   ,ppHiddenNoWindows = xmobarColor "#505050" "" . \s -> "[<fc=#586e75>+</fc>]"
   ,ppLayout          = xmobarColor green     "" .
      (\x -> case x of
       "Spacing 4 Tall" -> "[T]"
       "Spacing 4 Full" -> "<fc=#cb4b16>[F]</fc>"
      )
   ,ppTitle           = xmobarColor cyan          "" . shorten 75
   ,ppSep             = "  "
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
