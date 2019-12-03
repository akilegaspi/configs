{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
----------------------------------------------
-- My XMonad Configuration
-- Adrian King Legaspi (aki.legaspi@gmail.com)
-- https://github.com/akilegaspi
----------------------------------------------
-- XMonad Version: 0.14.2
----------------------------------------------

{-

** TODO **
- Figure out optimal settings for myself
- Create xmobar config with volume and network connection
- Customize workspace and make them visible on the xmobar

-}
import Control.Monad (liftM, liftM2, join, unless)  -- myManageHookShift
import Data.List
import qualified Data.Map as M
import Data.Monoid
import System.Exit
import System.IO                            -- for xmonbar
import System.Posix.Process(executeFile)

import XMonad hiding ( (|||) )                               -- ||| from X.L.LayoutCombinators
import qualified XMonad.StackSet as W       -- myManageHookShift

import XMonad.Actions.Commands
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow            -- like cylons, except x windows
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatSnap
import XMonad.Actions.MessageFeedback       -- pseudo conditional key bindings
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote               -- promote window to master
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll               -- action all the things

import XMonad.Hooks.DynamicLog              -- for xmobar
import XMonad.Hooks.DynamicProperty         -- 0.12 broken; works with github version
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks             -- avoid xmobar
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.Column
import XMonad.Layout.Combo
import XMonad.Layout.ComboP
import XMonad.Layout.DecorationMadness      -- testing alternative accordion styles
import XMonad.Layout.Dishes
import XMonad.Layout.DragPane
import XMonad.Layout.Drawer
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.OneBig
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.PerWorkspace           -- Configure layouts on a per-workspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile          -- Resizable Horizontal border
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing                -- this makes smart space around windows
import XMonad.Layout.StackTile
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts          -- Full window at any time
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

import XMonad.Prompt                        -- to get my old key bindings working
import XMonad.Prompt.ConfirmPrompt          -- don't just hard quit

import XMonad.Util.Cursor
import XMonad.Util.CustomKeys
import XMonad.Util.EZConfig                 -- removeKeys, additionalKeys
import XMonad.Util.Loggers
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Paste as P               -- testing
import XMonad.Util.Run                      -- for spawnPipe and hPutStrLn
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare         -- custom WS functions filtering NSP
import XMonad.Util.XSelection



import XMonad.Layout.Decoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders

-- Applications
myStatusBar    = "xmobar -x0 /home/aki/.xmobarrc"
myBrowser      = "firefox"
myBrowserClass = myBrowser
myLauncher     = "dmenu_run"
myTerminal     = "xterm"
-- Sizes
myBorderWidth  = 3
gap     = 10
topbar  = 0
prompt  = 20
status  = 20

main :: IO ()
main = do
  xmobProc <- spawnPipe myStatusBar
  xmonad $ myConfig xmobProc
  

myConfig p = def
  { borderWidth        = myBorderWidth
  , terminal           = myTerminal
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , logHook            = myLogHook p
  , manageHook         = myManageHook
  , handleEventHook    = myHandleEventHook
  , layoutHook         = myLayoutHook
--  , keys               = customKeys (const []) myKeys
  , mouseBindings      = myMouseBindings
  , startupHook        = myStartupHook
  , clickJustFocuses   = myClickJustFocuses
  , focusFollowsMouse  = myFocusFollowsMouse
  , modMask            = myModMask }

  
-- Spacings
mySpacing           = spacing gap
smolGap             = quot gap 2
myGaps              = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
mySmolGaps          = gaps [(U, smolGap),(D, smolGap),(L, smolGap),(R, smolGap)]
myBigGaps           = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]

-- Fonts
myFont = "xft:Inconsolata LGC:size=10:style=Medium"

-- Colors
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"
black   = "#000000"
white   = "#FFFFFF"


nBColor = "#2E3440"
fBColor = "#D8DEE9"

color0 = "#2b282b"
color1 = "#e06e94"
color2 = "#45b49a"
color3 = "#e5c3ce"
color4 = "#66998c"
color5 = "#7c7d82"
color6 = "#66988b"
color7 = "#b9bdc4"
color8 = "#2a282a"
color9 = "#e06e94"
color10 = "#83d0be"
color11 = "#e5c3ce"
color12 = "#62656c"
color13 = "#44b399"
color14 = "#004b32"
color15 = "#d4d8e0"
colorBg1 = "#0f0f0f"
colorBg2 = "#555555"
colorFg1 = "#bc23a5"
colorFg2 = "#cfc8f7"

type HexColor = String
type ColorPair = (HexColor, HexColor)
type ColorMap = M.Map Colors ColorPair

data Colors = Black
            | Red
            | Green
            | Yellow
            | Blue
            | Magenta
            | Cyan
            | White
            | BG
            | FG
deriving instance (Ord Colors)
deriving instance (Eq Colors)

colors :: ColorMap
colors = M.fromList
    [ (Black   , (color0,   color8  ))
    , (Red     , (color1,   color9  ))
    , (Green   , (color2,   color10 ))
    , (Yellow  , (color3,   color11 ))
    , (Blue    , (color4,   color12 ))
    , (Magenta , (color5,   color13 ))
    , (Cyan    , (color6,   color14 ))
    , (White   , (color7,   color15 ))
    , (BG      , (colorBg1, colorBg2))
    , (FG      , (colorFg1, colorFg2))
    ]

myColor :: Colors -> Int -> String
myColor color n = case M.lookup color colors of
                    Nothing -> "#000000"
                    Just (c1, c2) -> if n == 0 then c1 else c2




myNormalBorderColor  = myColor Cyan 1
myFocusedBorderColor = myColor Green 0

active        = violet
activeWarn    = magenta
inactive      = base01
focusColor    = violet
unfocuseColor = base01

myModMask = mod4Mask


-- Workspaces
wsAV    = "AV"
wsBSA   = "BSA"
wsCOM   = "COM"
wsDOM   = "DOM"
wsDMO   = "DMO"
wsFLOAT = "FLT"
wsGEN   = "GEN"
wsGCC   = "GCC"
wsMON   = "MON"
wsOSS   = "OSS"
wsRAD   = "RAD"
wsRW    = "RW"
wsSYS   = "SYS"
wsTMP   = "TMP"
wsVIX   = "VIX"
wsWRK   = "WRK"
wsWRK2  = "WRK:2"
wsGGC   = "GGC"


-- myWorkspaces = map show [1..9]
myWorkspaces = [wsGEN, wsWRK, wsWRK2, wsSYS, wsMON, wsFLOAT, wsRW, wsTMP]


projects :: [Project]
projects = [
  Project   { projectName       = wsGEN
            , projectDirectory  = "~/"
            , projectStartHook  = Nothing
            }
    
  , Project   { projectName       = wsSYS
              , projectDirectory  = "~/"
              , projectStartHook  = Just $ do
                    spawnOn wsSYS myTerminal
                    spawnOn wsSYS myTerminal
                    spawnOn wsSYS myTerminal
              }
    
  , Project   { projectName       = wsDMO
              , projectDirectory  = "~/"
                -- , projectStartHook  = Just $ do spawn "/usr/lib/xscreensaver/binaryring"
              , projectStartHook  = Just $ do
                  spawn "/usr/lib/xscreensaver/spheremonics"
                  runInTerm "-name top" "top"
                  runInTerm "-name top" "htop"
                  runInTerm "-name glances" "glances"
                  spawn "/usr/lib/xscreensaver/cubicgrid"
                  spawn "/usr/lib/xscreensaver/surfaces"
              }
  
  , Project   { projectName       = wsVIX
              , projectDirectory  = "~/.xmonad"
              , projectStartHook  = Just $ do
                  runInTerm "-name vix" "emacs ~/.xmonad/xmonad.hs"
                  spawnOn wsVIX myTerminal
                  spawnOn wsVIX myTerminal
              }
  
  , Project   { projectName       = wsMON
              , projectDirectory  = "~/"
              , projectStartHook  = Just $ do runInTerm "-name glances" "glances"
              }
  
  , Project   { projectName       = wsWRK
              , projectDirectory  = "~/wrk"
              , projectStartHook  = Just $ do
                  spawnOn wsWRK myTerminal
                  spawnOn wsWRK myBrowser
              }
  
  , Project   { projectName       = wsRAD
              , projectDirectory  = "~/"
              , projectStartHook  = Just $ do spawn myBrowser
              }
  
  , Project   { projectName       = wsTMP
              , projectDirectory  = "~/"
                -- , projectStartHook  = Just $ do spawn $ myBrowser ++ " https://mail.google.com/mail/u/0/#inbox/1599e6883149eeac"
              , projectStartHook  = Just $ do return ()
              }
  ]

barFull = avoidStruts $ Simplest
  
data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)


topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

myPromptTheme = def
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = prompt
    , position              = Top
    }

warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Top
    }

hotPromptTheme = myPromptTheme
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
    }

myShowWNameTheme = def
    { swn_font              = myFont
    , swn_fade              = 0.5
    , swn_bgcolor           = black
    , swn_color             = white
    }

scratchpads = [] 


showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()


---------------------------------------------------------------------------
-- Urgency Hook                                                            
---------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap (W.findTag w) $ gets windowset
  
    safeSpawn "notify-send" [show name, "workspace " ++ idx]

---------------------------------------------------------------------
-- Manage Hook
--------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook =
        manageSpecific
    <+> manageDocks
    <+> namedScratchpadManageHook scratchpads
    <+> fullscreenManageHook
    <+> manageSpawn
    where
        manageSpecific = composeOne
            [ resource =? "desktop_window" -?> doIgnore
            , resource =? "stalonetray"    -?> doIgnore
            , resource =? "mpv"    -?> doFloat
            , transience
            , isBrowserDialog -?> forceCenterFloat
            --, isConsole -?> forceCenterFloat
            , isRole =? gtkFile  -?> forceCenterFloat
            , isDialog -?> doCenterFloat
            , isRole =? "pop-up" -?> doCenterFloat
            , isInProperty "_NET_WM_WINDOW_TYPE"
                           "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat
            , resource =? "console" -?> tileBelowNoFocus
            , isFullscreen -?> doFullFloat
            , pure True -?> tileBelow ]
        isBrowserDialog = isDialog <&&> className =? myBrowserClass
        gtkFile = "GtkFileChooserDialog"
        isRole = stringProperty "WM_WINDOW_ROLE"
        -- insert WHERE and focus WHAT
        tileBelow = insertPosition Below Newer
        tileBelowNoFocus = insertPosition Below Older

---------------------------------------------------------------------------
-- X Event Actions
---------------------------------------------------------------------------

-- for reference, the following line is the same as dynamicTitle myDynHook
-- <+> dynamicPropertyChange "WM_NAME" myDynHook

-- I'm not really into full screens without my say so... I often like to
-- fullscreen a window but keep it constrained to a window rect (e.g.
-- for videos, etc. without the UI chrome cluttering things up). I can
-- always do that and then full screen the subsequent window if I want.
-- THUS, to cut a long comment short, no fullscreenEventHook
-- <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook

myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> handleEventHook def
                <+> removeBordersEventHook
                <+> XMonad.Layout.Fullscreen.fullscreenEventHook

--------------------------------------------------------------------------
-- Log Hook
--------------------------------------------------------------------------
myLogHook p = do
  copies <- wsContainingCopies
  let check ws | ws `elem` copies =
                 pad . xmobarColor (myColor Yellow 1) (myColor Red 1) . wrap "*" " " $ ws
               | otherwise = pad ws
  fadeWindowsLogHook myFadeHook
  ewmhDesktopsLogHook
  dynamicLogWithPP $ def
    { ppCurrent             = xmobarColor (myColor Green 1) "" . wrap "[" "]"
    , ppTitle               = xmobarColor (myColor Green 1) "" . shorten 50
    , ppVisible             = xmobarColor (myColor Yellow 1)  "" . wrap "(" ")"
    , ppUrgent              = xmobarColor (myColor Cyan 1)    "" . wrap " " " "
    , ppHidden              = check
    , ppHiddenNoWindows     = const ""
    , ppSep                 = xmobarColor (myColor Red 1) (myColor BG 1) "  :  "
    , ppWsSep               = " "
    , ppLayout              = xmobarColor (myColor Yellow 1) ""
    , ppOrder               = id
    , ppOutput              = hPutStrLn p
    , ppSort                = fmap
                              (namedScratchpadFilterOutWorkspace.)
                              (ppSort def)
                              --(ppSort defaultPP)
    , ppExtras              = [] }

------------------------------------------------------------------------
-- Layout Hook
------------------------------------------------------------------------
myLayoutHook = showWorkspaceName
             $ onWorkspace wsFLOAT floatWorkSpace
             $ fullscreenFloat -- fixes floating windows going full screen, while retaining "bounded" fullscreen
             $ fullScreenToggle
             $ fullBarToggle
             $ mirrorToggle
             $ reflectToggle
             $ flex ||| tabs
  where

--    testTall = Tall 1 (1/50) (2/3)
--    myTall = subLayout [] Simplest $ trackFloating (Tall 1 (1/20) (1/2))

    floatWorkSpace      = simplestFloat
    fullBarToggle       = mkToggle (single FULLBAR)
    fullScreenToggle    = mkToggle (single FULL)
    mirrorToggle        = mkToggle (single MIRROR)
    reflectToggle       = mkToggle (single REFLECTX)
    smallMonResWidth    = 1920
    showWorkspaceName   = showWName' myShowWNameTheme

    named n             = renamed [(XMonad.Layout.Renamed.Replace n)]
    trimNamed w n       = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                                   (XMonad.Layout.Renamed.PrependWords n)]
    suffixed n          = renamed [(XMonad.Layout.Renamed.AppendWords n)]
    trimSuffixed w n    = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                                   (XMonad.Layout.Renamed.AppendWords n)]

    addTopBar           = noFrillsDeco shrinkText topBarTheme

    mySpacing           = spacing gap
    sGap                = quot gap 2
    myGaps              = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
    mySmallGaps         = gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]
    myBigGaps           = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]

    --------------------------------------------------------------------------
    -- Tabs Layout                                                          --
    --------------------------------------------------------------------------

    threeCol = named "Unflexed"
         $ avoidStruts
         $ addTopBar
         $ myGaps
         $ mySpacing
         $ ThreeColMid 1 (1/10) (1/2)

    tabs = named "Tabs"
         $ avoidStruts
         $ addTopBar
         $ addTabs shrinkText myTabTheme
         $ Simplest

    flex = trimNamed 5 "Flex"
              $ avoidStruts
              -- don't forget: even though we are using X.A.Navigation2D
              -- we need windowNavigation for merging to sublayouts
              $ windowNavigation
              $ addTopBar
              $ addTabs shrinkText myTabTheme
              -- $ subLayout [] (Simplest ||| (mySpacing $ Accordion))
              $ subLayout [] (Simplest ||| Accordion)
              $ ifWider smallMonResWidth wideLayouts standardLayouts
              where
                  wideLayouts = myGaps $ mySpacing
                      $ (suffixed "Wide 3Col" $ ThreeColMid 1 (1/20) (1/2))
                    ||| (trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP)
                  --  ||| fullTabs
                  standardLayouts = myGaps $ mySpacing
                      $ (suffixed "Std 2/3" $ ResizableTall 1 (1/20) (2/3) [])
                    ||| (suffixed "Std 1/2" $ ResizableTall 1 (1/20) (1/2) [])
                    ||| (suffixed "Wide 3 Col" $ ThreeColMid 1 (1/20) (2/3) )

                  --  ||| fullTabs
                  --fullTabs = suffixed "Tabs Full" $ Simplest
                  --
                  -- NOTE: removed this from the two (wide/std) sublayout
                  -- sequences. if inside the ifWider, the ||| combinator
                  -- from X.L.LayoutCombinators can't jump to it directly (
                  -- or I'm doing something wrong, either way, it's simpler
                  -- to solve it by just using a tabbed layout in the main
                  -- layoutHook). The disadvantage is that I lose the "per
                  -- screen" memory of which layout was where if using the
                  -- tabbed layout (if using the the ifWider construct as
                  -- I am currently, it seems to work fine)
                  --
                  -- Using "Full" here (instead of Simplest) will retain the
                  -- tabbed sublayout structure and allow paging through each
                  -- group/window in full screen mode. However my preference
                  -- is to just see all the windows as tabs immediately.  
                  -- Using "Simplest" here will do this: display all windows
                  -- as tabs across the top, no "paging" required. However
                  -- this is misleading as the sublayouts are of course still
                  -- there and you will have to use the nornmal W.focusUp/Down
                  -- to successfully flip through them. Despite this
                  -- limitation I prefer this to the results with "Full".

{-|
    -----------------------------------------------------------------------
    -- Simple Flexi                                                      --
    -----------------------------------------------------------------------
    --
    -- Simple dynamically resizing layout as with the other variations in
    -- this config. This layout has not tabs in it and simply uses
    -- Resizable Tall and Three Column layouts.
    simpleFlexi = named "Simple Flexible"
              $ ifWider smallMonResWidth simpleThree simpleTall
    simpleTall = named "Tall"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ ResizableTall 1 (1/300) (2/3) []
              
    simpleThree = named "Three Col"
              $ avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ ThreeColMid 1 (3/100) (1/2)
    -----------------------------------------------------------------------
    -- Other Misc Layouts                                                --
    -----------------------------------------------------------------------
    --
    --
    masterTabbedP   = named "MASTER TABBED"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ mastered (1/100) (1/2) $ tabbed shrinkText myTabTheme
    bsp       = named "BSP"
              $ borderResize (avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ emptyBSP )
              -- $ borderResize (emptyBSP)
    oneBig    = named "1BG"
              $ avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ OneBig (3/4) (3/4)
    tiledP    = named "TILED"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ consoleOn
              $ tiled'
    oneUp =   named "1UP"
              $ avoidStruts
              $ myGaps
              $ combineTwoP (ThreeCol 1 (3/100) (1/2))
                            (Simplest)
                            (Tall 1 0.03 0.5)
                            (ClassName "Google-chrome-beta")
    -----------------------------------------------------------------------
    -- Master-Tabbed Dymamic                                             --
    -----------------------------------------------------------------------
    --
    -- Dynamic 3 pane layout with one tabbed panel using X.L.Master
    -- advantage is that it can do a nice 3-up on both ultrawide and
    -- standard (laptop in my case) screen sizes, where the layouts
    -- look like this:
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |  Master  |       Master       |   Tabs   |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    -- \____________________ _____________________/
    --                      '
    --                 all one layout
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |   Tabs   |
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- ---------------------------------
    -- \_______________ _______________/
    --                 '
    --            all one layout
    --
    -- Advantages to this use of X.L.Master to created this dynamic
    -- layout include:
    --
    --   * No fussing with special keys to swap windows between the
    --     Tabs and Master zones
    --
    --   * Window movement and resizing is very straightforward
    --
    --   * Limited need to maintain a mental-map of the layout
    --     (pretty easy to understand... it's just a layout)
    --
    -- Disadvantages include:
    --
    --   * Swapping a window from tabbed area will of necessity swap
    --     one of the Master windows back into tabs (since there can
    --     only be two master windows)
    --
    --   * Master area can have only one/two windows in std/wide modes
    --     respectively
    --
    --   * When switching from wide to standard, the leftmost pane
    --     (which is visually secondary to the large central master
    --     window) becomes the new dominant master window on the
    --     standard display (this is easy enough to deal with but
    --     is a non-intuitive effect)
    masterTabbedDynamic = named "Master-Tabbed Dynamic"
              $ ifWider smallMonResWidth masterTabbedWide masterTabbedStd
    masterTabbedStd = named "Master-Tabbed Standard"
              $ addTopBar
              $ avoidStruts
              $ gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]
              $ mastered (1/100) (2/3)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ tabbed shrinkText myTabTheme
    masterTabbedWide = named "Master-Tabbed Wide"
              $ addTopBar
              $ avoidStruts
              $ gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]
              $ mastered (1/100) (1/4)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ mastered (1/100) (2/3)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ tabbed shrinkText myTabTheme
    -----------------------------------------------------------------------
    -- Tall-Tabbed Dymamic                                               --
    -----------------------------------------------------------------------
    --
    -- Dynamic 3 pane layout with one tabbed panel using X.L.ComboP
    -- advantage is that it can do a nice 3-up on both ultrawide and
    -- standard (laptop in my case) screen sizes, where the layouts
    -- look like this:
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |----------|       Master       |   Tabs   |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    -- \______________ _______________/\____ _____/
    --                '                     '
    --        this set of panes is      This is a
    --        its' own layout in a      separate
    --        Tall configuration        tab format
    --                                  layout
LL    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |   Tabs   |
    -- |                    |          |
    -- |--------------------|          |
    -- |         |          |          |
    -- ---------------------------------
    -- \_________ _________/\____ _____/
    --           '               '
    -- this set of panes is  This is a
    -- its' own layout in a  separate
    -- Tall configuration    tab format
    --                       layout
    --
    -- Advantages to this use of ComboP to created this dynamic
    -- layout include:
    --
    --   * the center Master stays the same when the layout
    --     changes (unlike the X.L.Master based dyn. layout)
    --
    --   * the Master can have a set of panes under it on the
    --     small screen (standard) layout
    --
    --   * on ultrawide the leftmost pane may be divided into
    --     multiple windows
    --
    --   * possible to toss a tabbed window to the "Master" area
    --     without swapping a window back into tabs
    --
    --   * use of ComboP allows redirection windows to either
    --     left or right section
    --
    -- Disadvantages include:
    --
    --   * normal window swaps fail between the two separate
    --     layouts. There must be a special swap-between-layouts
    --     binding (normal window NAVIGATION works, at least using
    --     X.A.Navigation2D).
    --
    --   * switching between screens can leave title bar clutter
    --     that hasn't been cleaned up properly (restarting
    --     XMonad works to clean this up, but that's hacky)
    --
    --   * somewhat greater need to maintain a mental-map of the
    --     layout (you need to have a sense for the windows being
    --     in separate sections of the different layouts)
    smartTallTabbed = named "Smart Tall-Tabbed"
            $ avoidStruts
            $ ifWider smallMonResWidth wideScreen normalScreen
            where
            wideScreen   = combineTwoP (TwoPane 0.03 (3/4))
                           (smartTall)
                           (smartTabbed)
                           (ClassName "Google-chrome-beta")
            normalScreen = combineTwoP (TwoPane 0.03 (2/3))
                           (smartTall)
                           (smartTabbed)
                           (ClassName "Google-chrome-beta")
    smartTall = named "Smart Tall"
            $ addTopBar
        $ mySpacing
            $ myGaps
        $ boringAuto
            $ ifWider smallMonResWidth wideScreen normalScreen
            where
                wideScreen = reflectHoriz $ Tall 1 0.03 (2/3)
                normalScreen = Mirror $ Tall 1 0.03 (4/5)
    smartTabbed = named "Smart Tabbed"
              $ addTopBar
              $ myCustomGaps
              $ tabbed shrinkText myTabTheme
-}
    -----------------------------------------------------------------------
    -- Flexi Combinators                                                 --
    -----------------------------------------------------------------------
    --
    -- failed attempt. creates a nice looking layout but I'm not sure
    -- how to actually direct tabs to the tabbed area
    --
    --     flexiCombinators = named "Flexi Combinators"
    --             $ avoidStruts
    --             $ ifWider smallMonResWidth wideScreen normalScreen
    --             where
    --             wideScreen   = smartTall ****||* smartTabbed
    --             normalScreen = smartTall ***||** smartTabbed
wsKeys = map show $ [1..9] ++ [0]

-- any workspace but scratchpad
notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)

-- hidden, non-empty workspaces less scratchpad
shiftAndView' dir = findWorkspace getSortByIndexNoSP dir HiddenNonEmptyWS 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

-- toggle any workspace but scratchpad
myToggle = windows $ W.view =<< W.tag . head . filter 
  ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden

myKeys conf = let

    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    screenKeys     = ["w","v","z"]
    dirKeys        = ["j","k","h","l"]
    arrowKeys        = ["<D>","<U>","<L>","<R>"]
    dirs           = [ D,  U,  L,  R ]

    --screenAction f        = screenWorkspace >=> flip whenJust (windows . f)

    zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
    zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

    -- from xmonad.layout.sublayouts
    focusMaster' st = let (f:fs) = W.integrate st
        in W.Stack f [] fs
    swapMaster' (W.Stack f u d) = W.Stack f [] $ reverse u ++ d

    -- try sending one message, fallback if unreceived, then refresh
    tryMsgR x y = sequence_ [(tryMessage_ x y), refresh]

    -- warpCursor = warpToWindow (9/10) (9/10)

    -- cf https://github.com/pjones/xmonadrc
    --switch :: ProjectTable -> ProjectName -> X ()
    --switch ps name = case Map.lookup name ps of
    --  Just p              -> switchProject p
    --  Nothing | null name -> return ()

    -- do something with current X selection
    unsafeWithSelection app = join $ io $ liftM unsafeSpawn $ fmap (\x -> app ++ " " ++ x) getSelection

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

    in

    -----------------------------------------------------------------------
    -- System / Utilities
    -----------------------------------------------------------------------
    subKeys "System"
    [ ("M-q"                    , addName "Restart XMonad"                  $ spawn "xmonad --restart")
    , ("M-C-q"                  , addName "Rebuild & restart XMonad"        $ spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-q"                  , addName "Quit XMonad"                     $ confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))
    , ("M-x"                    , addName "Lock screen"                     $ spawn "xset s activate")
    , ("M-<F4>"                    , addName "Print Screen"                    $ return ())
  --, ("M-F1"                   , addName "Show Keybindings"                $ return ())
    ] ^++^

    -----------------------------------------------------------------------
    -- Actions
    -----------------------------------------------------------------------
    subKeys "Actions"
    [ ("M-a"                    , addName "Notify w current X selection"    $ unsafeWithSelection "notify-send")
  --, ("M-7"                    , addName "TESTING"                         $ runInTerm "-name glances" "glances" )
    , ("M-u"                    , addName "Copy current browser URL"        $ spawn "with-url copy")
    , ("M-o"                    , addName "Display (output) launcher"       $ spawn "displayctl menu")
    , ("M-<XF86Display>"        , addName "Display - force internal"        $ spawn "displayctl internal")
    , ("S-<XF86Display>"        , addName "Display - force internal"        $ spawn "displayctl internal")
    , ("M-i"                    , addName "Network (Interface) launcher"    $ spawn "nmcli_dmenu")
    , ("M-/"                    , addName "On-screen keys"                  $ spawn "killall screenkey &>/dev/null || screenkey --no-systray")
    , ("M-S-/"                  , addName "On-screen keys settings"         $ spawn "screenkey --show-settings")
    , ("M1-p"                   , addName "Capture screen"                  $ spawn "screenshot" )
    , ("M1-S-p"                 , addName "Capture screen - area select"    $ spawn "screenshot area" )
    , ("M1-r"                   , addName "Record screen"                   $ spawn "screencast" )
    , ("M1-S-r"                 , addName "Record screen - area select"     $ spawn "screencast area" )
    ] ^++^

    -----------------------------------------------------------------------
    -- Launchers
    -----------------------------------------------------------------------
    subKeys "Launchers"
    [ ("M-<Space>"              , addName "Launcher"                        $ spawn myLauncher)
    , ("M-<Return>"             , addName "Terminal"                        $ spawn myTerminal)
    , ("M-\\"                   , addName "Browser"                         $ spawn myBrowser)
    , ("M-c"                    , addName "NSP Chat"                        $ bindOn [(wsWRK, namedScratchpadAction scratchpads "hangoutsWork"),
                                                                                       ("", namedScratchpadAction scratchpads "hangoutsPersonal")])
    , ("M-t"                    , addName "NSP Tasks"                       $ bindOn [(wsWRK, namedScratchpadAction scratchpads "trelloWork"),
                                                                                       ("", namedScratchpadAction scratchpads "trello")])
    , ("M-m"                    , addName "NSP Music"                       $ namedScratchpadAction scratchpads "googleMusic")
    , ("M-v"                    , addName "NSP Video"                       $ namedScratchpadAction scratchpads "plex")
    , ("M1-x"                   , addName "NSP Xawtv"                       $ namedScratchpadAction scratchpads "xawtv")
    , ("M-n"                    , addName "NSP Console"                     $ namedScratchpadAction scratchpads "console")
    , ("M-s s"                  , addName "Cancel submap"                   $ return ())
    , ("M-s M-s"                , addName "Cancel submap"                   $ return ())
    ] ^++^

    -----------------------------------------------------------------------
    -- Windows
    -----------------------------------------------------------------------

    subKeys "Windows"
    (
    [ ("M-<Backspace>"          , addName "Kill"                            kill1)
    , ("M-S-<Backspace>"        , addName "Kill all"                        $ confirmPrompt hotPromptTheme "kill all" $ killAll)
    --, ("M-d"                    , addName "Duplicate w to all ws"           $ windows copyToAll)
    --, ("M-S-d"                  , addName "Kill other duplicates"           $ killAllOtherCopies)
    , ("M-d"                    , addName "Duplicate w to all ws"           $ toggleCopyToAll)
    , ("M-p"                    , addName "Hide window to stack"            $ withFocused hideWindow)
    , ("M-S-p"                  , addName "Restore hidden window (FIFO)"    $ popOldestHiddenWindow)

    , ("M-b"                    , addName "Promote"                         $ promote) 

    , ("M-g"                    , addName "Un-merge from sublayout"         $ withFocused (sendMessage . UnMerge))
    , ("M-S-g"                  , addName "Merge all into sublayout"        $ withFocused (sendMessage . MergeAll))

    , ("M-z u"                  , addName "Focus urgent"                    focusUrgent)
    , ("M-z m"                  , addName "Focus master"                    $ windows W.focusMaster)

    --, ("M-<Tab>"              	, addName "Focus down"                      $ windows W.focusDown)
    --, ("M-S-<Tab>"              , addName "Focus up"                        $ windows W.focusUp)

    , ("M-'"                    , addName "Navigate tabs D"                 $ bindOn [("Tabs", windows W.focusDown), ("", onGroup W.focusDown')])
    , ("M-;"                    , addName "Navigate tabs U"                 $ bindOn [("Tabs", windows W.focusUp), ("", onGroup W.focusUp')])
    , ("C-'"                    , addName "Swap tab D"                      $ windows W.swapDown)
    , ("C-;"                    , addName "Swap tab U"                      $ windows W.swapUp)

    -- ComboP specific (can remove after demo)
    , ("M-C-S-m"                , addName "Combo swap"                      $ sendMessage $ SwapWindow)
    ]

    ++ zipM' "M-"               "Navigate window"                           dirKeys dirs windowGo True
    -- ++ zipM' "M-S-"               "Move window"                               dirKeys dirs windowSwap True
    -- TODO: following may necessitate use of a "passthrough" binding that can send C- values to focused w
    ++ zipM' "C-"             "Move window"                               dirKeys dirs windowSwap True
    ++ zipM  "M-C-"             "Merge w/sublayout"                         dirKeys dirs (sendMessage . pullGroup)
    ++ zipM' "M-"               "Navigate screen"                           arrowKeys dirs screenGo True
    -- ++ zipM' "M-S-"             "Move window to screen"                     arrowKeys dirs windowToScreen True
    ++ zipM' "M-C-"             "Move window to screen"                     arrowKeys dirs windowToScreen True
    ++ zipM' "M-S-"             "Swap workspace to screen"                  arrowKeys dirs screenSwap True

    ) ^++^

    -----------------------------------------------------------------------
    -- Workspaces & Projects
    -----------------------------------------------------------------------

    -- original version was for dynamic workspaces
    --    subKeys "{a,o,e,u,i,d,...} focus and move window between workspaces"
    --    (  zipMod "View      ws" wsKeys [0..] "M-"      (withNthWorkspace W.greedyView)

    subKeys "Workspaces & Projects"
    (
    [ ("M-w"                    , addName "Switch to Project"           $ switchProjectPrompt warmPromptTheme)
    , ("M-S-w"                  , addName "Shift to Project"            $ shiftToProjectPrompt warmPromptTheme)
    , ("M-<Escape>"             , addName "Next non-empty workspace"    $ nextNonEmptyWS)
    , ("M-S-<Escape>"           , addName "Prev non-empty workspace"    $ prevNonEmptyWS)
    , ("M-`"                    , addName "Next non-empty workspace"    $ nextNonEmptyWS)
    , ("M-S-`"                  , addName "Prev non-empty workspace"    $ prevNonEmptyWS)
    , ("M-a"                    , addName "Toggle last workspace"       $ toggleWS' ["NSP"])
    ]
    ++ zipM "M-"                "View      ws"                          wsKeys [0..] (withNthWorkspace W.greedyView)
    -- ++ zipM "M-S-"              "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    -- TODO: following may necessitate use of a "passthrough" binding that can send C- values to focused w
    ++ zipM "C-"                "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    -- TODO: make following a submap
    ++ zipM "M-S-C-"            "Copy w to ws"                          wsKeys [0..] (withNthWorkspace copy)
    ) ^++^

    -- TODO: consider a submap for nav/move to specific workspaces based on first initial

    -----------------------------------------------------------------------
    -- Layouts & Sublayouts
    -----------------------------------------------------------------------

    subKeys "Layout Management"

    [ ("M-<Tab>"                , addName "Cycle all layouts"               $ sendMessage NextLayout)
    , ("M-C-<Tab>"              , addName "Cycle sublayout"                 $ toSubl NextLayout)
    , ("M-S-<Tab>"              , addName "Reset layout"                    $ setLayout $ XMonad.layoutHook conf)

    , ("M-y"                    , addName "Float tiled w"                   $ withFocused toggleFloat)
    , ("M-S-y"                  , addName "Tile all floating w"             $ sinkAll)

    , ("M-,"                    , addName "Decrease master windows"         $ sendMessage (IncMasterN (-1)))
    , ("M-."                    , addName "Increase master windows"         $ sendMessage (IncMasterN 1))

    , ("M-r"                    , addName "Reflect/Rotate"              $ tryMsgR (Rotate) (XMonad.Layout.MultiToggle.Toggle REFLECTX))
    , ("M-S-r"                  , addName "Force Reflect (even on BSP)" $ sendMessage (XMonad.Layout.MultiToggle.Toggle REFLECTX))


    -- If following is run on a floating window, the sequence first tiles it.
    -- Not perfect, but works.
    , ("M-f"                , addName "Fullscreen"                      $ sequence_ [ (withFocused $ windows . W.sink)
                                                                        , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) ])

    -- Fake fullscreen fullscreens into the window rect. The expand/shrink
    -- is a hack to make the full screen paint into the rect properly.
    -- The tryMsgR handles the BSP vs standard resizing functions.
    , ("M-S-f"                  , addName "Fake fullscreen"             $ sequence_ [ (P.sendKey P.noModMask xK_F11)
                                                                                    , (tryMsgR (ExpandTowards L) (Shrink))
                                                                                    , (tryMsgR (ExpandTowards R) (Expand)) ])
    , ("C-S-h"                  , addName "Ctrl-h passthrough"          $ P.sendKey controlMask xK_h)
    , ("C-S-j"                  , addName "Ctrl-j passthrough"          $ P.sendKey controlMask xK_j)
    , ("C-S-k"                  , addName "Ctrl-k passthrough"          $ P.sendKey controlMask xK_k)
    , ("C-S-l"                  , addName "Ctrl-l passthrough"          $ P.sendKey controlMask xK_l)
    ] ^++^

    -----------------------------------------------------------------------
    -- Reference
    -----------------------------------------------------------------------
    -- recent windows not working
    -- , ("M4-<Tab>",              , addName "Cycle recent windows"        $ (cycleRecentWindows [xK_Super_L] xK_Tab xK_Tab))
    -- either not using these much or (in case of two tab items below), they conflict with other bindings
    -- so I'm just turning off this whole section for now. retaining for refernce after a couple months
    -- of working with my bindings to see if I want them back. TODO REVIEW
    --, ("M-s m"                  , addName "Swap master"                 $ windows W.shiftMaster)
    --, ("M-s p"                  , addName "Swap next"                   $ windows W.swapUp)
    --, ("M-s n"                  , addName "Swap prev"                   $ windows W.swapDown)
    --, ("M-<Tab>"                , addName "Cycle up"                    $ windows W.swapUp)
    --, ("M-S-<Tab>"              , addName "Cycle down"                  $ windows W.swapDown)

    -- sublayout specific (unused)
    -- , ("M4-C-S-m"               , addName "onGroup focusMaster"         $ onGroup focusMaster')
    -- , ("M4-C-S-]"               , addName "toSubl IncMasterN 1"         $ toSubl $ IncMasterN 1)
    -- , ("M4-C-S-["               , addName "toSubl IncMasterN -1"        $ toSubl $ IncMasterN (-1))
    -- , ("M4-C-S-<Return>"        , addName "onGroup swapMaster"          $ onGroup swapMaster')


    -----------------------------------------------------------------------
    -- Resizing
    -----------------------------------------------------------------------

    subKeys "Resize"

    [

    -- following is a hacky hack hack
    --
    -- I want to be able to use the same resize bindings on both BinarySpacePartition and other
    -- less sophisticated layouts. BSP handles resizing in four directions (amazing!) but other
    -- layouts have less refined tastes and we're lucky if they just resize the master on a single
    -- axis.
    --
    -- To this end, I am using X.A.MessageFeedback to test for success on using the BSP resizing
    -- and, if it fails, defaulting to the standard (or the X.L.ResizableTile Mirror variants)
    -- Expand and Shrink commands.
    --
    -- The "sequence_" wrapper is needed because for some reason the windows weren't resizing till
    -- I moved to a different window or refreshed, so I added that here. Shrug.
    
    -- mnemonic: less than / greater than
    --, ("M4-<L>"       , addName "Expand (L on BSP)"     $ sequence_ [(tryMessage_ (ExpandTowards L) (Expand)), refresh])

--      ("C-<L>"                  , addName "Expand (L on BSP)"           $ tryMsgR (ExpandTowards L) (Shrink))
--    , ("C-<R>"                  , addName "Expand (R on BSP)"           $ tryMsgR (ExpandTowards R) (Expand))
--    , ("C-<U>"                  , addName "Expand (U on BSP)"           $ tryMsgR (ExpandTowards U) (MirrorShrink))
--    , ("C-<D>"                  , addName "Expand (D on BSP)"           $ tryMsgR (ExpandTowards D) (MirrorExpand))
--
--    , ("C-S-<L>"                , addName "Shrink (L on BSP)"           $ tryMsgR (ShrinkFrom R) (Shrink))
--    , ("C-S-<R>"                , addName "Shrink (R on BSP)"           $ tryMsgR (ShrinkFrom L) (Expand))
--    , ("C-S-<U>"                , addName "Shrink (U on BSP)"           $ tryMsgR (ShrinkFrom D) (MirrorShrink))
--    , ("C-S-<D>"                , addName "Shrink (D on BSP)"           $ tryMsgR (ShrinkFrom U) (MirrorExpand))

      ("M-["                    , addName "Expand (L on BSP)"           $ tryMsgR (ExpandTowards L) (Shrink))
    , ("M-]"                    , addName "Expand (R on BSP)"           $ tryMsgR (ExpandTowards R) (Expand))
    , ("M-S-["                  , addName "Expand (U on BSP)"           $ tryMsgR (ExpandTowards U) (MirrorShrink))
    , ("M-S-]"                  , addName "Expand (D on BSP)"           $ tryMsgR (ExpandTowards D) (MirrorExpand))

    , ("M-C-["                  , addName "Shrink (L on BSP)"           $ tryMsgR (ShrinkFrom R) (Shrink))
    , ("M-C-]"                  , addName "Shrink (R on BSP)"           $ tryMsgR (ShrinkFrom L) (Expand))
    , ("M-C-S-["                , addName "Shrink (U on BSP)"           $ tryMsgR (ShrinkFrom D) (MirrorShrink))
    , ("M-C-S-]"                , addName "Shrink (D on BSP)"           $ tryMsgR (ShrinkFrom U) (MirrorExpand))

  --, ("M-r"                    , addName "Mirror (BSP rotate)"         $ tryMsgR (Rotate) (XMonad.Layout.MultiToggle.Toggle MIRROR))
  --, ("M-S-C-m"                , addName "Mirror (always)"             $ sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
  --, ("M4-r"                   , addName "BSP Rotate"                  $ sendMessage Rotate)

-- TODO: the following are potentially useful but I won't know till I work with BSP further
--    , ("M4-s"                   , addName "BSP Swap"                    $ sendMessage XMonad.Layout.BinarySpacePartition.Swap)
--    , ("M4-p"                   , addName "BSP Focus Parent"            $ sendMessage FocusParent)
--    , ("M4-n"                   , addName "BSP Select Node"             $ sendMessage SelectNode)
    --, ("M4-m"                   , addName "BSP Move Node"               $ sendMessage MoveNode)

    -- sublayout specific (unused)
    --  ("M4-C-S-."               , addName "toSubl Shrink"               $ toSubl Shrink)
    --, ("M4-C-S-,"               , addName "toSubl Expand"               $ toSubl Expand)
    ]
	where
          toggleCopyToAll = wsContainingCopies >>=
            \ws -> case ws of
              [] -> windows copyToAll
              _ -> killAllOtherCopies

    -----------------------------------------------------------------------
    -- Screens
    -----------------------------------------------------------------------
--    subKeys "Screens"
--    ([("M-C-<Right>", addName "Focus prev screen" prevScreen)
--    , ("M-C-<Left>" , addName "Focus next screen" nextScreen)
--    ]
--    ++ zipMod "Focus screen"                         screenKeys [0..] "M-"    (screenAction W.view)
--    ++ zipMod "Move client to screen"                screenKeys [0..] "M-S-"  (screenAction W.shift)
--    ++ zipMod "Swap workspace with screen"           screenKeys [0..] "M-M1-" (screenAction W.greedyView)
--    ++ zipMod "Swap workspace with and focus screen" screenKeys [0..] "M-C-"  (\s -> screenAction W.greedyView s >> screenAction W.view s)
--    ) ^++^

--    subKeys "Media Controls"
--    [
--    ("<XF86AudioMicMute>"      , addName "Mic Mute"                    $ spawn "notify-send mic mute")
--    ]

myMouseBindings (XConfig {XMonad.modMask = myModMask}) = M.fromList $

    [ ((myModMask,               button1) ,(\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicMove (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button1), (\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask,               button3), (\w -> focus w
      >> mouseResizeWindow w
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button3), (\w -> focus w
      >> Sqr.mouseResizeWindow w True
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster ))

--    , ((mySecondaryModMask,      button4), (\w -> focus w
--      >> prevNonEmptyWS))
--
--    , ((mySecondaryModMask,      button5), (\w -> focus w
--      >> nextNonEmptyWS))

    ]
------------------------------------------------------------------------}}}
-- Startup                                                              {{{
---------------------------------------------------------------------------

myStartupHook = do

    -- init-tilingwm sets up all major "desktop environment" like components
    -- spawnOnce "$HOME/bin/wm/init-tilingwm"
    -- spawn "/home/ethan/bin/wm/init-tilingwm"
    spawn "feh --bg-scale $HOME/wp/vpw.jpg"
    spawn "xcompmgr -cCfF"

    -- init-tray kills and restarts stalone tray, hence just "spawn" so it
    -- runs on restart and will suffice to reposition tray on display changes
    -- TODO: evaluate moving to a "restart tray only" option on display change
    -- spawn     "$HOME/bin/wm/init-tray"

    setDefaultCursor xC_left_ptr

quitXmonad :: X ()
quitXmonad = io (exitWith ExitSuccess)

rebuildXmonad :: X ()
rebuildXmonad = do
    spawn "xmonad --recompile && xmonad --restart"

restartXmonad :: X ()
restartXmonad = do
  spawn "xmonad --restart"

------------------------------------------------------------------------}}}
-- Fade Hook                                                            {{{
---------------------------------------------------------------------------
myFadeHook = composeAll
    [ isUnfocused --> opacity 0.85
    , isDialog --> opaque 
    , isUnfocused --> opacity 0.55
    , isFloating  --> opacity 0.75
    , (className =? "XTerm") <&&> (isUnfocused) --> opacity 0.9
    , (className =? "URxvt") <&&> (isUnfocused) --> opacity 0.9
    , fmap ("Firefox" `isPrefixOf`) className --> opaque
    , opaque
    ]

removeBordersEventHook :: Event -> X All
removeBordersEventHook ev = do
  whenX (className =? "mpv" `runQuery` w) $ withDisplay $ \d -> do
    cw <- io $ wa_border_width <$> getWindowAttributes d w
    unless (cw == 0) $ do
      io $ setWindowBorderWidth d w 0
      refresh
  return (All True)
  where
    w = ev_window ev

myClickJustFocuses :: Bool
myClickJustFocuses  = True

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Utils

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 1/2
    x = (1 - w) / 2
    y = (1 - h) / 2
