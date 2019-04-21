--{-# OPTIONS -Wall -Werror #-}

import Prelude

import Control.Monad
import Data.List
import Data.Monoid
import Data.Ord
import System.Directory
import System.Posix.Files

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig
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

screenB :: Border
screenB = Border b b b b

windowB :: Border
windowB = Border b b b b

myLayout =   toggleLayouts Full
           $ mouseResize
           $ spacingRaw False screenB True windowB True
           $ ResizableTall 1 0.01 0.5 [] ||| Full

xbacklight :: String -> String
xbacklight = ("xbacklight " ++) . (++ " -time 1")

amixer :: String -> String
amixer = ("amixer set Master " ++)

myBrowser :: String
myBrowser = "chromium"

myKeysP :: [(String, X ())]
myKeysP = [ ("M-p"       , spawn "rofi -show run")
          , ("M-u"       , spawn myTerminal)
          , ("M-s"       , spawn myBrowser)
          , (  "<Print>" , spawn "screenshot.sh 0.7 60"          )
          , ("S-<Print>" , spawn "screenshot.sh 0.7 60 --focused")
          , ("M-h"       , moveTo   Prev NonEmptyWS)
          , ("M-l"       , moveTo   Next NonEmptyWS)
          , ("M-n"       , do
                             windows $ S.greedyView "9"
                             moveTo Next    EmptyWS)
          , ("M-f"       , do
                             sendMessage ToggleLayout
                             sendMessage ToggleStruts)
          , ("M-<Left>"  , sendMessage Shrink)
          , ("M-<Right>" , sendMessage Expand)
          , ("M-<Up>"    , sendMessage MirrorExpand)
          , ("M-<Down>"  , sendMessage MirrorShrink)
          , ("M-S-="     , decScreenWindowSpacing 2)
          , ("M--"       , incScreenWindowSpacing 2)
          , ("M-0"       , do
                             setScreenSpacing screenB
                             setWindowSpacing windowB)
          , ("M-c"       , kill)
          , ("<XF86MonBrightnessUp>"  ,   spawn $ xbacklight "+5"  )
          , ("<XF86MonBrightnessDown>",   spawn $ xbacklight "-5"  )
          , ("S-<XF86MonBrightnessUp>"  , spawn $ xbacklight "+100")
          , ("S-<XF86MonBrightnessDown>", spawn $ xbacklight "-100")
          , ("<XF86AudioRaiseVolume>" , spawn $ amixer "1%+"   )
          , ("<XF86AudioLowerVolume>" , spawn $ amixer "1%-"   )
          , ("<XF86AudioMute>"        , spawn $ amixer "toggle")
          , ("M-S-c"       , return ())
          , ("M-S-<Return>", return ())
          , ("M-<Tab>"     , return ())
          , ("M-S-<Tab>"   , return ())
          ]

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def <+> fullscreenEventHook <+> ewmhDesktopsEventHook

myManageHook :: ManageHook
myManageHook =
   manageDocks <+> composeAll
   [ className =? "feh"              --> doCenterFloat
   , className =? "jetbrains-studio" --> doFloat
   , className =? "jetbrains-idea"   --> doFloat
   , className =? "Galculator"       --> doCenterFloat
   , className =? "polybar"       --> doCenterFloat
   , isFullscreen                    --> doFullFloat
   , isDialog                        --> doCenterFloat
   ]

getWorkspaceLog :: X String
getWorkspaceLog = do
      winset <- gets windowset
      let currWs = S.currentTag winset
      let wss    = S.workspaces winset
      let wsIds  = map S.tag   $ wss
      let wins   = map S.stack $ wss
      let (wsIds', wins') = sortById wsIds wins
      return . join . map (fmt currWs wins') $ wsIds'
      where
         hasW = not . null
         idx = flip (-) 1 . read
         sortById ids xs = unzip $ sortBy (comparing fst) (zip ids xs)
         fmt cw ws id
              | id == cw            = "\63022"
              | hasW $ ws !! idx id = "\61842"
              | otherwise           = "\63023"

eventLogHook :: FilePath -> X ()
eventLogHook filename = io . appendFile filename . (++ "\n") =<< getWorkspaceLog

main :: IO ()
main = do
      de <- doesFileExist wsLogfile
      case de of
         True -> return ()
         _    -> createNamedPipe wsLogfile stdFileMode
      xmonad . ewmh $ myConfig wsLogfile
      where
         wsLogfile = "/tmp/.xmonad-workspace-log"

myConfig filename = def
   { terminal        = myTerminal
   , modMask         = myModMask
   , borderWidth     = myBorderWidth
   , layoutHook      = myLayout
   , manageHook      = myManageHook
   , logHook         = eventLogHook filename
   , handleEventHook = myHandleEventHook
   } `additionalKeysP` myKeysP
