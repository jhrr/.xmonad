import XMonad


myTerminal          = "urxvt"
myModMask           = mod4Mask
myBorderWidth       = 2
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myWorkspaces        = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

main = do
  xmonad $ defaultConfig
    { terminal          = myTerminal
    , modMask           = myModMask
    , borderWidth       = myBorderWidth
    , focusFollowsMouse = myFocusFollowsMouse
    , workspaces        = myWorkspaces
    }
