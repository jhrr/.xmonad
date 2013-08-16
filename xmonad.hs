import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.WorkspaceNames
import XMonad.Util.Run(spawnPipe)
import System.IO


myTerminal          = "urxvt"
myModMask           = mod4Mask
myBorderWidth       = 2

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myWorkspaces 	    = ["α", "β" ,"γ", "δ", "ε", "ζ", "η", "θ", "ι"]

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
	    , layoutHook = avoidStruts $ layoutHook defaultConfig
	    , logHook    = dynamicLogWithPP xmobarPP
	                       { ppOutput          = hPutStrLn xmproc
			               , ppTitle  	       = xmobarColor "green" "" . shorten 100
			               , ppHidden          = xmobarColor "white" ""
			               , ppHiddenNoWindows = xmobarColor "gray" ""
			               }
        , terminal          = myTerminal
    	, modMask           = myModMask
    	, borderWidth       = myBorderWidth
    	, focusFollowsMouse = myFocusFollowsMouse
    	, workspaces        = myWorkspaces
	}
