import XMonad
import qualified Data.Map as M
import XMonad.Operations (unGrab)
import Graphics.X11.ExtraTypes.XF86
import Prelude hiding (magenta, blue, white, yellow, red)
-- Layout
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Dwindle
-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.SetWMName
-- Actions
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer
-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Shell
-- Utils
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Loggers (logTitles)
import XMonad.ManageHook
import qualified XMonad.StackSet as W


myTerminal :: String
myTerminal = "ghostty" 
-- myTerminal = "alacritty" -- Alacritty or Ghostty

-- Open a ghostty terminal and create or attach to session jr. It should just
-- attach since we create the tmux session in our startup hook
createTerminalCmd :: String
createTerminalCmd = myTerminal ++ " +new-window -e tmux new -A -s jr"
-- createTerminalCmd = myTerminal ++ " msg create-window"

myBrowser :: String
myBrowser = "firefox-devedition"

myFont :: String
myFont = "xft:DejaVuSansM Nerd Font Mono:weight=regular:pixelsize=16:antialias=true:hinting=true"

myConfig = def
  { terminal   = createTerminalCmd
  , manageHook = myManageHook
  , startupHook = myStartupHook
  , layoutHook = smartSpacing 2 $ myLayout
  , borderWidth = 2
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#cd8b00"
  }
  `additionalKeysP`
    [ ("M-S-l", spawn "xscreensaver-command -lock")
    , ("M-S-s", spawn "systemctl suspend")
    , ("M-S-p", spawn "scrot -s -q 100 ~/Pictures/Screenshots/screenshot-%Y-%m-%d_%H:%M:%S.png")
    , ("M-f", spawn myBrowser)
    -- , ("M-p", spawn "rofi -show run")
    , ("M-a", openScratchpad "terminal")
    , ("M-o", openScratchpad "obsidian")
    , ("M-e", openScratchpad "emacs")
    , ("M-m", openScratchpad "spotify")
    , ("M-S-v", openScratchpad "vpn")
    , ("M-t", shellPrompt myPromptConfig)
    , ("M-w", withFocused $ toggleFloat $ rectCentered 0.9)
    , ("M-S-w", withFocused $ toggleFloat $ vertRectCentered 0.95)
    , ("M-S-b", bringMenu)
    , ("M-g", gotoMenu)
    ]
  `additionalKeys`
    [ ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 2%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 2%+")
    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 10%")
    ]

--        spawn = createTerminalCmd ++ " --title ghostty-scratchpad -e tmux new -s scratchpad -A"
myScratchpads :: [NamedScratchpad]
myScratchpads = [terminal, obsidian, emacs, spotify, protonvpn]
  where
    terminal = NS "terminal" spawn find manage
      where
        spawn = myTerminal ++ " +new-window --title=ghostty-scratchpad -e tmux new -s scratchpad -A"
        find = title =? "ghostty-scratchpad"
        manage = customFloating $ vertRectCentered 0.9
    obsidian = NS "obsidian" spawn find manage
      where
        spawn = "obsidian"
        find = className =? "obsidian"
        manage = customFloating $ vertRectCentered 0.8
    emacs = NS "emacs" spawn find manage
      where
        spawn = "emacsclient -r -F '((title . \"emacs-scratchpad\"))'"
        find = title =? "emacs-scratchpad"
        manage = customFloating $ vertRectCentered 0.95
    spotify = NS "spotify" spawn find manage
      where
        spawn = "spotify"
        find = className =? "Spotify"
        manage = customFloating $ vertRectCentered 0.90
    protonvpn = NS "vpn" spawn find manage
      where
        spawn = "protonvpn-app"
        find = title =? "Proton VPN"
        manage = customFloating $ vertRectCentered 0.90

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

vertRectCentered :: Rational -> W.RationalRect
vertRectCentered height = W.RationalRect offsetX offsetY width height
  where
    width = height / 1.5
    offsetX = (1 - width) / 2
    offsetY = (1 - height) / 2

myPromptConfig :: XPConfig
myPromptConfig = def {
  font = myFont 
  , bgColor = background $ primary colors
  , fgColor = foreground $ primary colors
  , bgHLight = yellow $ normal colors
  , promptBorderWidth = 0
  , position = Bottom
  , height = 28
  , maxComplRows = Just 5
  , showCompletionOnTab = True
}

toggleFloat :: W.RationalRect -> Window -> X ()
toggleFloat r w =
  windows
    ( \s ->
      if M.member w (W.floating s)
        then W.sink w s
        else W.float w r s
    )

colors :: Colors
colors =
  Colors
    { primary = PrimaryColors {background = "#282828", foreground = "#dfbf8e"},
      normal =
        RegularColors
          { black = "#665c54",
            blue = "#7daea3",
            cyan = "#89b482",
            green = "#a9b665",
            magenta = "#d3869b",
            red = "#ea6962",
            white = "#dfbf8e",
            yellow = "#e78a4e"
          }
    }
    
data Colors = Colors
  { primary :: PrimaryColors,
    normal :: RegularColors
  }

data PrimaryColors = PrimaryColors
  { background :: String,
    foreground :: String
  }

data RegularColors = RegularColors
  { black :: String,
    blue :: String,
    cyan :: String,
    green :: String,
    magenta :: String,
    red :: String,
    white :: String,
    yellow :: String
  }

myStartupHook :: X ()
myStartupHook = composeAll
  [ setWMName "jr"
  , spawnOn "1" "emacs --daemon"
  , spawn "tmux has-session -t jr 2>/dev/null || tmux new -d -s jr -n workspace -c ~/workspace \\; new-window -n scratch -c ~/workspace \\; new-window -c ~/workspace"
  ]
  -- , spawnOn "1" "alacritty --daemon"

myLayout = dwindle ||| dwindleR ||| tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled    = ResizableTall nmaster delta ratio []
    dwindle  = Dwindle L CCW 1.5 1.1
    dwindleR = Dwindle R CW 1.5 1.1
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

-- XMobar configuration using the new statusBar API
myXmobarPP :: PP
myXmobarPP = def
  { ppSep             = magenta " • "
  , ppTitleSanitize   = xmobarStrip
  , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#cd8b00" 2
  , ppHidden          = white . wrap " " ""
  , ppHiddenNoWindows = lowWhite . wrap " " ""
  , ppUrgent          = red . wrap (yellow "!") (yellow "!")
  , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
  , ppExtras          = [logTitles formatFocused formatUnfocused]
  }
  where
    formatFocused   = wrap (white    "[") (white    "]") . yellow . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue   . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if Prelude.null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor (Main.magenta $ normal colors) ""
    blue     = xmobarColor (Main.blue $ normal colors) ""
    white    = xmobarColor (Main.white $ normal colors) ""
    yellow   = xmobarColor (Main.yellow $ normal colors) ""
    red      = xmobarColor (Main.red $ normal colors) ""
    lowWhite = xmobarColor "#bbbbbb" ""

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig
