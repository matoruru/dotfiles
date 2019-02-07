import XMonad
import System.IO
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger
import XMonad.Layout.Fullscreen
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
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
myNormalBorderColor  = base0
myFocusedBorderColor = cyan
myBorderWidth        = 0

gap_UD = 7
gap_LR = 21
gapwidth = gap_UD
gwU      = gap_UD
gwD      = gap_UD
gwL      = gap_LR
gwR      = gap_LR
myLayout = fullscreenFull $
           toggleLayouts Full $
           mouseResize $
           windowArrange $
           lessBorders OnlyFloat $
           spacing gapwidth $
           gaps [(U, gwU), (D, gwD), (L, gwL), (R, gwR)] $
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
   fullscreenManageHook <+>
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

myBar = "xmobar"

myXmobarPP = xmobarPP
   {
    ppCurrent         = xmobarColor darkgreen "" . \s -> "[<fc=#96aa44>$</fc>]"
   ,ppHidden          = xmobarColor darkgreen "" . \s -> "[ ]"
   ,ppHiddenNoWindows = xmobarColor "#505050" "" . \s -> "[<fc=#586e75>+</fc>]"
   ,ppLayout          = xmobarColor green     "" .
      (\x -> case x of
       "Spacing 7 ResizableTall" ->             "[T]"
       "Spacing 7 Full"          -> "<fc=#cb4b16>[F]</fc>"
      )
   ,ppTitle           = xmobarColor cyan          "" . shorten 70
   ,ppSep             = "  "
   ,ppWsSep           = ""
   }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myStartupHook = do
   spawn "wmname LG3D"

main = do
   xmonad =<< statusBar myBar myXmobarPP toggleStrutsKey myConfig

myConfig = def
   {
    terminal           = myTerminal
   ,modMask            = myModMask
   ,normalBorderColor  = myNormalBorderColor
   ,focusedBorderColor = myFocusedBorderColor
   ,borderWidth        = myBorderWidth
   ,layoutHook         = myLayout
   ,startupHook        = myStartupHook
   ,manageHook         = myManageHook
   ,handleEventHook    = fullscreenEventHook
   } `additionalKeysP` myKeysP
