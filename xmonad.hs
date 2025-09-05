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


main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig

myConfig = def
    { terminal   = "ghostty"
    , layoutHook = smartSpacing 2 $ myLayout
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-s", spawn "systemctl suspend")
    , ("M-S-o", spawn "obsidian")
    , ("M-d", spawn "dbeaver-ce")
    , ("M-f", spawn "zen")
    , ("M-s", spawn "slack")
    , ("M-m", spawn "spotify")
    , ("M-z", spawn "zoom")
    , ("M-S-p", spawn "scrot -s")
    ]
  `additionalKeys`
    [ ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 2%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 2%+")
    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((0, xF86XK_MonBrightnessUp), spawn "backlightctl -set 2%+")
    , ((0, xF86XK_MonBrightnessDown), spawn "backlightctl -set 2%+")
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled    = ResizableTall nmaster delta ratio []
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
