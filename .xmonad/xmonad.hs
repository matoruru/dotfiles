{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE LambdaCase #-}

import RIO.List
import RIO.Prelude

import Data.Monoid

import System.Directory
import System.Posix.Files

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WallpaperSetter
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W

main :: IO ()
main = do
      doesFileExist wsLogfile >>= \case
         True  -> return ()
         False -> createNamedPipe wsLogfile stdFileMode
      xmonad . ewmh . docks $ myConfig wsLogfile

myConfig :: FilePath -> XConfig (ModifiedLayout AvoidStruts (ModifiedLayout Spacing (Choose ResizableTall Full)))
myConfig filename = def
   { terminal        = myTerminal
   , modMask         = myModMask
   , workspaces      = myWorkspaces
   , borderWidth     = myBorderWidth
   , layoutHook      = myLayout
   , manageHook      = myManageHook
   , logHook         = myLogHook filename
   , handleEventHook = myHandleEventHook
   , startupHook     = myStartupHook
   } `additionalKeysP`     myKeysP
     `removeMouseBindings` myKeysToRemove

myTerminal :: String
myTerminal = "kitty"

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 0

border :: Border
border = (\x -> Border x x x x) 9

myLayout :: Eq a => ModifiedLayout AvoidStruts (ModifiedLayout Spacing (Choose ResizableTall Full)) a
myLayout = avoidStruts
         $ spacingRaw False border True border True
         $ ResizableTall 1 0.01 0.5 [] ||| Full

myBrowser, appLauncher, screenShot, screenShot', reStart, reCompile :: X ()
myBrowser   = spawn "chromium"
appLauncher = spawn "rofi -show run"
screenShot  = spawn "screenshot.sh 0.7 60"
screenShot' = spawn "screenshot.sh 0.7 60 --focused"
reStart     = spawn "xmonad --restart"
reCompile   = spawn "xmonad --recompile && xmonad --restart"

bLight, amixer :: String -> X ()
bLight = spawn . ("xbacklight " ++) . (++ " -time 1")
amixer = spawn . ("amixer set Master " ++)

myKeysP :: [(String, X ())]
myKeysP = [ ("M-u"      , spawn myTerminal)
          , ("M-s"      , myBrowser)
          , ("M-p"      , appLauncher)
          , ("<Print>"  , screenShot)
          , ("S-<Print>", screenShot')
          , ("M-w"  , setWallpaper >> reStart)
          , ("M-S-m", windows W.swapMaster)
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
          , ("M-S-=", decScreenWindowSpacing 4)
          , ("M--"  , incScreenWindowSpacing 4)
          , ("M-<XF86ApplicationRight>" , bLight "+5"  )
          , ("<XF86MonBrightnessUp>"    , bLight "+5"  )
          , ("<XF86MonBrightnessDown>"  , bLight "-5"  )
          , ("S-<XF86MonBrightnessUp>"  , bLight "+100")
          , ("S-<XF86MonBrightnessDown>", bLight "-100")
          , ("<XF86AudioRaiseVolume>", amixer "1%+"   )
          , ("<XF86AudioLowerVolume>", amixer "1%-"   )
          , ("<XF86AudioMute>"       , amixer "toggle")
          , ("M-S-c"       , return ())
          , ("M-<Return>"  , return ())
          , ("M-S-<Return>", return ())
          , ("M-S-r", reCompile)
          , ("M-r"  , reStart  )
          , ("M-S-q", return() )
          , ("M-q"  , return() )
          , ("M-c", kill)
          ]

myKeysToRemove :: [(ButtonMask, Button)]
myKeysToRemove = [ (mod4Mask, button1)
                 , (mod4Mask, button2)
                 , (mod4Mask, button3)
                 ]

myHandleEventHook :: Event -> X All
myHandleEventHook = composeAll
         [ handleEventHook def
         , fullscreenEventHook
         , ewmhDesktopsEventHook
         ]

myManageHook :: ManageHook
myManageHook = composeAll
         [ className =? "feh"              --> doCenterFloat
         , className =? "jetbrains-studio" --> doFloat
         , className =? "jetbrains-idea"   --> doFloat
         , className =? "Galculator"       --> doCenterFloat
         , isFullscreen                    --> doFullFloat
         , isDialog                        --> doCenterFloat
         ]

data WsState
  = Here
  | NotEmpty
  | Empty

getWorkspaceLog :: X String
getWorkspaceLog = do
      winset <- gets windowset
      let currWs = W.currentTag winset
          wss    = W.workspaces winset
          wsIds  = map W.tag   $ wss
          wins   = map W.stack $ wss
          (wsIds', wins') = sortById wsIds wins
      return . join . map (wsState . fmt currWs wins') $ wsIds'
      where
         idx          = flip (-) 1 . read
         sortById ids = unzip . sortBy (comparing fst) . zip ids
         fmt cw ws wi
            | wi == cw              = Here
            | isJust $ ws !! idx wi = NotEmpty
            | otherwise             = Empty

wsState :: WsState -> String
wsState = \case
  Here     -> "\63022"
  NotEmpty -> "\61842"
  Empty    -> "\63023"

writeWorkspaceLog :: FilePath -> X ()
writeWorkspaceLog filename = io . appendFile filename . (++ "\n") =<< getWorkspaceLog

myLogHook :: FilePath -> X ()
myLogHook filename = writeWorkspaceLog filename

myWallpaperDir :: IO FilePath
myWallpaperDir = (++ "/Pictures/wallpapers/") <$> getHomeDirectory

myWallpaper :: FilePath
myWallpaper = "main.jpg"

setWallpaper :: X ()
setWallpaper = do
      wallpaperDir <- io $ myWallpaperDir
      let wplist = WallpaperList $ zip myWorkspaces $ wallpapers'
          wpconf = (WallpaperConf wallpaperDir) wplist
      wallpaperSetter wpconf
          where wallpapers' = repeat $ WallpaperFix myWallpaper

wsLogfile :: FilePath
wsLogfile = "/tmp/.xmonad-workspace-log"

myStartupHook :: X ()
myStartupHook = setDefaultCursor xC_left_ptr >> setWMName "LG3D" >> setWallpaper

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Int]
