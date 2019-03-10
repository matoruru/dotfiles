import XMonad
import System.IO
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import qualified XMonad.StackSet as W

-- Solarized color codes
base03   = "#002b36"
base02   = "#073642"
base01   = "#586e75"
base00   = "#657b83"
base0    = "#839496"
base1    = "#93a1a1"
base2    = "#eee8d5"
base3    = "#fdf6e3"
yellow   = "#b58900"
orange   = "#cb4b16"
red      = "#dc322f"
magenta  = "#d33682"
violet   = "#6c71c4"
blue     = "#268bd2"
cyan     = "#2aa198"
green    = "#859900"

darkgreen    = "#748800"
lightgreen   = "#96aa44"

myTerminal    = "kitty"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 0

ub = 21
b  = 9
myLayout = toggleLayouts Full $
           mouseResize $
           windowArrange $
           spacingRaw False (Border (b + ub) b b b) True (Border b b b b) True $
           ResizableTall 1 0.01 0.5 [] ||| Full

xbacklight x = "xbacklight " ++ x ++ " -time 1"
amixer     x = "amixer set Master " ++ x

myBrowser = "chromium"

myKeysP = [
           ("M-p"         , spawn "rofi -show run")
          ,("M-u"         , spawn myTerminal)
          ,("M-s"         , spawn myBrowser)
          ,(  "<Print>"   , spawn "screenshot.sh 0.7 60"          )
          ,("S-<Print>"   , spawn "screenshot.sh 0.7 60 --focused")
          ,("M-h"         , moveTo   Prev NonEmptyWS)
          ,("M-l"         , moveTo   Next NonEmptyWS)
          ,("M-n"         , do
                              windows $ W.greedyView "9"
                              moveTo Next    EmptyWS)
          ,("M-f"         , do
                              sendMessage ToggleLayout
                              sendMessage ToggleStruts)
          ,("M-<Left>"    , sendMessage Shrink)
          ,("M-<Right>"   , sendMessage Expand)
          ,("M-<Up>"      , sendMessage MirrorExpand)
          ,("M-<Down>"    , sendMessage MirrorShrink)
          ,("M-c"         , kill)
          ,("<XF86MonBrightnessUp>"  ,   spawn (xbacklight "+5"  ))
          ,("<XF86MonBrightnessDown>",   spawn (xbacklight "-5"  ))
          ,("S-<XF86MonBrightnessUp>"  , spawn (xbacklight "+100"))
          ,("S-<XF86MonBrightnessDown>", spawn (xbacklight "-100"))
          ,("<XF86AudioRaiseVolume>" , spawn (amixer "1%+"   ))
          ,("<XF86AudioLowerVolume>" , spawn (amixer "1%-"   ))
          ,("<XF86AudioMute>"        , spawn (amixer "toggle"))
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

myManageHook =
   manageDocks <+>
   composeAll
   [
-- How to get className, xprop|grep WM_CLASS, and click the window
    className =? "feh"               --> doCenterFloat
   ,className =? "jetbrains-studio" --> doFloat
   ,className =? "jetbrains-idea"    --> doFloat
   ,isFullscreen --> doFullFloat
   ,isDialog     --> doCenterFloat
   ]

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myStartupHook = do
      setWMName "LG3D"
      spawn "bash ~/.config/polybar/launch.sh"

myHandleEventHook =
          fullscreenEventHook
      <+> ewmhDesktopsEventHook

main = xmonad myConfig

myConfig = def
   {
    terminal           = myTerminal
   ,modMask            = myModMask
   ,borderWidth        = myBorderWidth
   ,layoutHook         = myLayout
   ,startupHook        = myStartupHook
   ,manageHook         = myManageHook
   ,handleEventHook    = myHandleEventHook
   } `additionalKeysP` myKeysP
