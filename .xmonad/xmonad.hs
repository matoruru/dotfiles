--{-# OPTIONS -Wall -Werror #-}

import Data.Ord (comparing)
import Data.List
import Data.Monoid
import Control.Monad (join)

import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn)
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import qualified XMonad.StackSet as S


myTerminal :: String
myTerminal = "kitty"

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 0

ub :: Integer
ub = 21

b :: Integer
b  = 9

myLayout = toggleLayouts Full $
           mouseResize $
           windowArrange $
           spacingRaw False (Border (b + ub) b b b) True (Border b b b b) True $
           ResizableTall 1 0.01 0.5 [] ||| Full

xbacklight :: String -> String
xbacklight x = "xbacklight " ++ x ++ " -time 1"

amixer :: String -> String
amixer     x = "amixer set Master " ++ x

myBrowser :: String
myBrowser = "chromium"

myKeysP :: [(String, X ())]
myKeysP = [ ("M-p"         , spawn "rofi -show run")
          , ("M-u"         , spawn myTerminal)
          , ("M-s"         , spawn myBrowser)
          , (  "<Print>"   , spawn "screenshot.sh 0.7 60"          )
          , ("S-<Print>"   , spawn "screenshot.sh 0.7 60 --focused")
          , ("M-h"         , moveTo   Prev NonEmptyWS)
          , ("M-l"         , moveTo   Next NonEmptyWS)
          , ("M-n"         , do
                               windows $ S.greedyView "9"
                               moveTo Next    EmptyWS)
          , ("M-f"         , do
                               sendMessage ToggleLayout
                               sendMessage ToggleStruts)
          , ("M-<Left>"    , sendMessage Shrink)
          , ("M-<Right>"   , sendMessage Expand)
          , ("M-<Up>"      , sendMessage MirrorExpand)
          , ("M-<Down>"    , sendMessage MirrorShrink)
          , ("M-S-="       , decScreenWindowSpacing 2)
          , ("M--"         , incScreenWindowSpacing 2)
          , ("M-0"         , do
                               setScreenSpacing (Border (b + ub) b b b)
                               setWindowSpacing (Border b b b b))
          , ("M-c"         , kill)
          , ("<XF86MonBrightnessUp>"  ,   spawn (xbacklight "+5"  ))
          , ("<XF86MonBrightnessDown>",   spawn (xbacklight "-5"  ))
          , ("S-<XF86MonBrightnessUp>"  , spawn (xbacklight "+100"))
          , ("S-<XF86MonBrightnessDown>", spawn (xbacklight "-100"))
          , ("<XF86AudioRaiseVolume>" , spawn (amixer "1%+"   ))
          , ("<XF86AudioLowerVolume>" , spawn (amixer "1%-"   ))
          , ("<XF86AudioMute>"        , spawn (amixer "toggle"))
          , ("M-S-c"       , return ())
          , ("M-S-<Return>", return ())
          , ("M-<Tab>"     , return ())
          , ("M-S-<Tab>"   , return ())
          ]

myManageHook :: ManageHook
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

myHandleEventHook :: Event -> X All
myHandleEventHook =
          fullscreenEventHook
      <+> ewmhDesktopsEventHook

eventLogHook :: X ()
eventLogHook = do
      winset <- gets windowset
      let currWs = S.currentTag winset
      let wss    = S.workspaces winset
      let wsIds  = map S.tag   $ wss
      let wins   = map S.stack $ wss
      let (wsIds', wins') = sortById wsIds wins
      let output = join . map (fmt currWs wins') $ wsIds'
      io $ appendFile "/tmp/.xmonad-workspace-log" (output ++ "\n")
      where
         hasW = not . null
         idx = flip (-) 1 . read
         sortById ids xs = unzip $ sortBy (comparing fst) (zip ids xs)
         fmt cw si ws
              | ws == cw            = "\63022"
              | hasW $ si !! idx ws = "\61842"
              | otherwise           = "\63023"

main :: IO ()
main = do
      safeSpawn "mkfifo" ["/tmp/.xmonad-workspace-log"]
      xmonad myConfig

myConfig = def { terminal           = myTerminal
               ,modMask            = myModMask
               ,borderWidth        = myBorderWidth
               ,layoutHook         = myLayout
               ,manageHook         = myManageHook
               ,logHook            = eventLogHook
               ,handleEventHook    = myHandleEventHook
               } `additionalKeysP` myKeysP
