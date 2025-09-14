import XMonad

import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Operations (unGrab)
import Graphics.X11.ExtraTypes.XF86
-- Layout
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.SetWMName
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS
-- Scratchpads
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook
import XMonad.StackSet as W


main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig

myConfig = def
  { terminal   = "alacritty" -- Alacritty or Ghostty
  , manageHook = myManageHook
  , startupHook = myStartupHook
  , layoutHook = smartSpacing 2 $ myLayout
  }
  `additionalKeysP`
    [ ("M-S-l", spawn "xscreensaver-command -lock")
    , ("M-S-s", spawn "systemctl suspend")
    , ("M-S-o", spawn "obsidian")
    , ("M-S-p", spawn "scrot -s")
    , ("M-S-n", spawn "nm-connection-editor")
    , ("M-d", spawn "dbeaver-ce")
    , ("M-f", spawn "zen")
    , ("M-s", spawn "slack")
    , ("M-m", spawn "spotify")
    , ("M-z", spawn "zoom")
    , ("M-p", spawn "rofi -show run")
    , ("M-a", openScratchpad "terminal")
    ]
  `additionalKeys`
    [ ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 2%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 2%+")
    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((0, xF86XK_MonBrightnessUp), spawn "backlightctl -set 2%+")
    , ((0, xF86XK_MonBrightnessDown), spawn "backlightctl -set 2%+")
    ]

myScratchpads :: [NamedScratchpad]
myScratchpads = [terminal]
  where
    terminal = NS "terminal" spawn find manage
      where
        spawn = "alacritty --class scratchpad -e tmux new -s scratchpad -A"
        find = className =? "scratchpad"
        manage = customFloating $ rectCentered 0.6

openScratchpad :: String -> X ()
openScratchpad = namedScratchpadAction myScratchpads

myManageHook :: ManageHook
myManageHook = composeAll
  [ namedScratchpadManageHook myScratchpads
  ]

rectCentered :: Rational -> W.RationalRect
rectCentered percentage = W.RationalRect offset offset percentage percentage
  where
    offset = (1 - percentage) / 2

myStartupHook :: X ()
myStartupHook = composeAll
  [ setWMName "jr"
  , spawnOn "1" "alacritty --daemon"
  ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled    = ResizableTall nmaster delta ratio []
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
