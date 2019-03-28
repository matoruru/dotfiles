--{-# OPTIONS -Wall -Werror #-}

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)

import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn)
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import qualified XMonad.StackSet as W

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
          ]

myManageHook =
   manageDocks <+>
   composeAll
   [
-- How to get className, xprop|grep WM_CLASS, and click the window
    className =? "feh"               --> doCenterFloat
   ,className =? "jetbrains-studio" --> doFloat
   ,className =? "jetbrains-idea"    --> doFloat
   ,className =? "Galculator"    --> doCenterFloat
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

eventLogHook = do
      winset <- gets windowset
      let currWs = W.currentTag winset
      let wss = map W.tag $ W.workspaces winset
      let wsStr  = join $ map (fmt currWs) $ sort' wss
      io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")
      where fmt currWs ws
              | currWs == ws = "\63022"
              | otherwise    = "\63023"
            sort' = sortBy (compare `on` (!! 0))

main = do
      forM_ [".xmonad-workspace-log"] $ \file -> do
         safeSpawn "mkfifo" ["/tmp/" ++ file]
      xmonad myConfig

myConfig = def
   {
    terminal           = myTerminal
   ,modMask            = myModMask
   ,borderWidth        = myBorderWidth
   ,layoutHook         = myLayout
   ,startupHook        = myStartupHook
   ,manageHook         = myManageHook
   ,logHook            = eventLogHook
   ,handleEventHook    = myHandleEventHook
   } `additionalKeysP` myKeysP
