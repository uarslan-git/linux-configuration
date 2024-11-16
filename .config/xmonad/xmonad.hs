import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Fullscreen
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run

main :: IO ()
main = do
    n <- countScreens
    xmprocs <- mapM (\i -> spawnPipe $ "xmobar /home/user/.xmobarrc-" ++ show i ++ " -x " ++ show i) [0..n-1]
    xmonad $ ewmhFullscreen $ ewmh $  xmobarProp $ docks def
        { modMask            = mod4Mask
        , terminal           = "alacritty"
        , borderWidth        = 2
        , normalBorderColor  = "#282c34"
        , focusedBorderColor = "#61afef"
        , startupHook        = myStartupHook
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , logHook            = dynamicLogString def >>= xmonadPropLog
        } `additionalKeysP` myKeys

myStartupHook = do
    spawnOnce "nitrogen --restore"
    spawnOnce "picom"
    spawn "systemctl --user restart i3-session.target"

myLayout = avoidStruts $ toggleLayouts Full $ spacing 8 $ Tall 1 (3/100) (1/2)

myManageHook = composeAll
    [ className =? "Firefox" --> doShift "2"
    , className =? "Gimp" --> doFloat
    , className =? "Steam" --> doFloat
    , className =? "Steam" <&&> isFullscreen --> doFloat
    , isFullscreen --> doFloat
    ]

myKeys =
    [ ("M-S-<Return>", spawn "alacritty")
    , ("M-d", spawn "rofi -show drun -show-icons")
    , ("M-S-p", spawn "thunar")
    , ("M-S-p", spawn "QT_SCALE_FACTOR=0.75 keepassxc")
    , ("M-S-;", spawn "copyq show")
    , ("M-S-s", spawn "flameshot gui")
    , ("M-S-w", spawn "loginctl lock-session")
    , ("M-S-c", kill)  -- Close focused window
    , ("M-b", sendMessage ToggleStruts)
    , ("M-f", sendMessage (Toggle "Full"))
    ]
