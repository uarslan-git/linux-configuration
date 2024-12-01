import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.IndependentScreens
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run
import Graphics.X11.Xlib
import Graphics.X11.Xinerama
import Graphics.X11.Xrandr
import System.Process (callCommand)
import XMonad.Layout.ThreeColumns

main :: IO ()
main = do
    n <- countScreens
    xmprocs <- mapM (\i -> spawnPipe $ "xmobar /home/user/.xmobarrc" ++ " -x " ++ show i) [0..n-1]

    xmonad $ ewmhFullscreen . ewmh $  xmobarProp $ def
        { modMask            = mod4Mask
        , terminal           = "alacritty"
        , borderWidth        = 1
		, focusFollowsMouse  = True
        , normalBorderColor  = "#282c34"
		, handleEventHook    = fullscreenEventHook
        , focusedBorderColor = "#61afef"
        , startupHook        = myStartupHook >> setWMName "LG3D"
        , layoutHook         = myLayout
        } `additionalKeysP` myKeys

myStartupHook = do
    spawnOnce "keepassxc"
    spawnOnce "flameshot"
    spawnOnce "nm-applet"
    spawnOnce "copyq"
    spawn "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --height 22"
    spawn "feh --bg-fill ~/.config/bg.jpg"
    spawn "picom"

myLayout = toggleLayouts  Full $ spacing 8 $ Tall 1 (3/100) (1/2)

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Firefox" --> doShift "2"
    , className =? "Gimp" --> doFloat
    , isFullscreen --> doFloat
    ]

myKeys =
    [ ("M-S-<Return>", spawn "alacritty")
    , ("M-d", spawn "rofi -show drun -show-icons")
    , ("M-S-p", spawn "thunar")
    , ("M-S-o", spawn "keepassxc")
    , ("M-S-;", spawn "copyq show")
    , ("M-S-i", spawn "pavucontrol")
    , ("M-S-s", spawn "flameshot gui")
    , ("M-S-l", spawn "lock")
    , ("M-S-c", kill)  -- Close focused window
    , ("M-b", sendMessage ToggleStruts)
    , ("M-f", sendMessage (Toggle "Full"))
    , ("M-q", spawn "xmonad --recompile; pkill xmobar; pkill trayer; xmonad --restart")
    ]

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
