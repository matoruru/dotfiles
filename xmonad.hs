import XMonad
import System.IO
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
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
myBorderWidth        = 0

gap_UD = 10
gap_LR = 22
gapwidth = gap_UD
gwU      = gap_UD
gwD      = gap_UD
gwL      = gap_LR
gwR      = gap_LR
myLayout = lessBorders OnlyFloat $
           spacing gapwidth $
           gaps [(U, gwU), (D, gwD), (L, gwL), (R, gwR)] $
           Tall 1 0.01 0.5 ||| Full

myKeysP = [
           ("M-p"         , spawn "rofi -show run")
          ,("M-u"         , spawn "urxvtc"             )
          ,("M-S-u"       , spawn "urxvtc -e sudo su -")
          ,("M-v"         , spawn "urxvtc -e vimtutor")
          ,("M-s"         , spawn "qutebrowser")
          ,(  "<Print>"   , spawn "screenshot.sh 0.7 60"          )
          ,("S-<Print>"   , spawn "screenshot.sh 0.7 60 --focused")
          ,("M-h"         , moveTo   Prev NonEmptyWS)
          ,("M-l"         , moveTo   Next NonEmptyWS)
          ,("M-n"         , do
                              windows $ W.greedyView "9"
                              moveTo Next    EmptyWS)
          ,("M-<Left>"    , sendMessage Shrink)
          ,("M-<Right>"   , sendMessage Expand)
          ,("M-c"         , kill)
          ,("<XF86MonBrightnessUp>"  ,   spawn "xbacklight +5 -time 10")
          ,("<XF86MonBrightnessDown>",   spawn "xbacklight -5 -time 10")
          ,("S-<XF86MonBrightnessUp>"  , spawn "xbacklight +100 -time 10")
          ,("S-<XF86MonBrightnessDown>", spawn "xbacklight -100 -time 10")
          ,("<XF86AudioRaiseVolume>" , spawn "amixer set Master 1%+"   )
          ,("<XF86AudioLowerVolume>" , spawn "amixer set Master 1%-"   )
          ,("<XF86AudioMute>"        , spawn "amixer set Master toggle")
          ,("M-S-c"       , spawn "")
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

myManageHook = manageFloat <+> manageDocks
manageFloat  = composeAll
   [ title =? "feh [1 of 1] - _____scReeNshoT-tmP.png" --> doCenterFloat ]

myBar = "xmobar"

myXmobarPP = xmobarPP
   {
    ppCurrent         = xmobarColor darkgreen "" . \s -> "[<fc=#96aa44>$</fc>]"
   ,ppHidden          = xmobarColor darkgreen "" . \s -> "[ ]"
   ,ppHiddenNoWindows = xmobarColor "#505050" "" . \s -> "[<fc=#586e75>+</fc>]"
   ,ppLayout          = xmobarColor green     "" .
      (\x -> case x of
       "Spacing 10 Tall" ->             "[T]"
       "Spacing 10 Full" -> "<fc=#cb4b16>[F]</fc>"
      )
   ,ppTitle           = xmobarColor cyan          "" . shorten 70
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
   ,manageHook         = myManageHook
   } `additionalKeysP` myKeysP
