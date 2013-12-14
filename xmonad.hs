import XMonad
import XMonad.ManageHook()
import XMonad.Actions.WorkspaceNames()
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doCenterFloat, doFullFloat)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.StackSet as W
import System.IO


myTerminal          = "urxvt"
myBorderWidth       = 2
myModMask           = mod4Mask
myWorkspaces        = ["α", "β" ,"γ", "δ", "ε", "ζ", "η", "θ", "ι"]
myFocusFollowsMouse = True


myManageHook :: ManageHook
myManageHook = composeAll . concat $
   [ [ className =? "Emacs" --> doShift "β" ]
   , [ className =? "Chromium" --> doShift "γ" ]
   , [ className =? "Firefox" --> doShift "γ" ]
   , [ className =? "Vlc" --> doShift "θ" ]
   , [ className =? "Skype" --> doShift "ι" ]
   , [ isFullscreen --> doFullFloat]
   , [ isDialog --> doCenterFloat] ]


myKeys :: [ (String, X()) ]
myKeys =  [ ("M-g" , spawn "firefox")
          , ("M-v" , spawn "vlc")
          -- , ("M-<xK_Print>" , spawn "scrot")
          -- , ("M-S-<xK_Print>" , spawn "sleep 0.2; scrot -s")
          , ("M-h" , spBeckon "htop")
          ]


myLogHook :: Handle -> X ()
myLogHook xmproc = do
    dynamicLogWithPP $ xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "green" "" . shorten 100
        , ppHidden = xmobarColor "white" "" . noScratchPad
        , ppHiddenNoWindows = xmobarColor "gray" ""
        }
        where
          noScratchPad ws = if ws == "NSP" then "" else ws


main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook        = manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads <+> manageHook defaultConfig
        , layoutHook        = avoidStruts $ layoutHook defaultConfig
        , logHook           = myLogHook xmproc
        , terminal          = myTerminal
        , modMask           = myModMask
        , borderWidth       = myBorderWidth
        , focusFollowsMouse = myFocusFollowsMouse
        , XMonad.workspaces = myWorkspaces
	} `additionalKeysP` myKeys


-- Scratchpads
spBeckon :: String -> X ()
spBeckon = namedScratchpadAction scratchpads

centeredFloat :: Rational -> Rational -> ManageHook
centeredFloat widthProportion heightProportion =
  customFloating $ W.RationalRect l t widthProportion heightProportion
    where t = (1 - heightProportion)/2
          l = (1 - widthProportion)/2

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "htop" "urxvt -e htop" (title =? "htop") (centeredFloat 0.8 0.8) ]
