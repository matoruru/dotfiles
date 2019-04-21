{-# OPTIONS -Wall -Werror #-}

import Prelude

import Control.Monad
import Data.List
import Data.Monoid
import Data.Ord
import System.Directory
import System.Posix.Files

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W


myTerminal :: String
myTerminal = "kitty"

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 0

b :: Integer
b  = 9

border :: Border
border = Border b b b b


myLayout :: Eq a => ModifiedLayout AvoidStruts (ModifiedLayout Spacing (Choose ResizableTall Full)) a
myLayout = avoidStruts
         $ spacingRaw False border True border True
         $ ResizableTall 1 0.01 0.5 [] ||| Full

xbacklight :: String -> String
xbacklight = ("xbacklight " ++) . (++ " -time 1")

amixer :: String -> String
amixer = ("amixer set Master " ++)

myBrowser :: String
myBrowser = "chromium"

myKeysP :: [(String, X ())]
myKeysP = [ ("M-s"      , spawn myBrowser)
          , ("M-u"      , spawn myTerminal)
          , ("M-p"      , spawn "rofi -show run")
          , ("<Print>"  , spawn "screenshot.sh 0.7 60"          )
          , ("S-<Print>", spawn "screenshot.sh 0.7 60 --focused")
          , ("M-h"  , moveTo Prev NonEmptyWS)
          , ("M-l"  , moveTo Next NonEmptyWS)
          , ("M-S-l", moveTo Next EmptyWS   )
          , ("M-S-h", moveTo Prev EmptyWS   )
          , ("M-<Tab>"  , nextScreen)
          , ("M-S-<Tab>", prevScreen)
          , ("M-<L>", sendMessage Shrink      )
          , ("M-<R>", sendMessage Expand      )
          , ("M-<U>", sendMessage MirrorExpand)
          , ("M-<D>", sendMessage MirrorShrink)
          , ("M-0"  , do setScreenSpacing border
                         setWindowSpacing border)
          , ("M-S-=", decScreenWindowSpacing 4  )
          , ("M--"  , incScreenWindowSpacing 4  )
          , ("M-<XF86ApplicationRight>"    , spawn $ xbacklight "+5"  )
          , ("<XF86MonBrightnessUp>"    , spawn $ xbacklight "+5"  )
          , ("<XF86MonBrightnessDown>"  , spawn $ xbacklight "-5"  )
          , ("S-<XF86MonBrightnessUp>"  , spawn $ xbacklight "+100")
          , ("S-<XF86MonBrightnessDown>", spawn $ xbacklight "-100")
          , ("<XF86AudioRaiseVolume>", spawn $ amixer "1%+"   )
          , ("<XF86AudioLowerVolume>", spawn $ amixer "1%-"   )
          , ("<XF86AudioMute>"       , spawn $ amixer "toggle")
          , ("M-c", kill)
          , ("M-S-c"       , return ())
          , ("M-S-<Return>", return ())
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
   , isFullscreen                    --> doFullFloat
   , isDialog                        --> doCenterFloat
   ]

getWorkspaceLog :: X String
getWorkspaceLog = do
      winset <- gets windowset
      let currWs = W.currentTag winset
      let wss    = W.workspaces winset
      let wsIds  = map W.tag   $ wss
      let wins   = map W.stack $ wss
      let (wsIds', wins') = sortById wsIds wins
      return . join . map (fmt currWs wins') $ wsIds'
      where
         hasW            = not . null
         idx             = flip (-) 1 . read
         sortById ids xs = unzip $ sortBy (comparing fst) (zip ids xs)
         fmt cw ws wi
              | wi == cw            = "\63022"
              | hasW $ ws !! idx wi = "\61842"
              | otherwise           = "\63023"

myLogHook :: FilePath -> X ()
myLogHook filename = io . appendFile filename . (++ "\n") =<< getWorkspaceLog

myStartupHook :: X ()
myStartupHook = do
      setDefaultCursor xC_left_ptr
      setWMName "LG3D"

main :: IO ()
main = do
      de <- doesFileExist wsLogfile
      case de of
         True -> return ()
         _    -> createNamedPipe wsLogfile stdFileMode
      xmonad . ewmh . docks $ myConfig wsLogfile
      where
         wsLogfile = "/tmp/.xmonad-workspace-log"

myConfig :: FilePath -> XConfig (ModifiedLayout AvoidStruts (ModifiedLayout Spacing (Choose ResizableTall Full)))
myConfig filename = def
   { terminal        = myTerminal
   , modMask         = myModMask
   , borderWidth     = myBorderWidth
   , layoutHook      = myLayout
   , manageHook      = myManageHook
   , logHook         = myLogHook filename
   , handleEventHook = myHandleEventHook
   , startupHook     = myStartupHook
   } `additionalKeysP` myKeysP `removeMouseBindings` [(mod4Mask, button1), (mod4Mask, button2), (mod4Mask, button3)]
