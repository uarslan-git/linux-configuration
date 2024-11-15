import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Layout.Spacing (spacing)

main :: IO ()
main = xmonad $ ewmhFullscreen $ docks def
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
    ]

myKeys =
    [ ("M-S-<Return>", spawn "alacritty")
    , ("M-p", spawn "dmenu_run")
    , ("M-S-c", kill)  -- Close focused window
    , ("M-b", sendMessage ToggleStruts)
    , ("M-f", sendMessage (Toggle "Full"))
    ]
