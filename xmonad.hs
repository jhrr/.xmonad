import XMonad
import XMonad.ManageHook()
import XMonad.Actions.WorkspaceNames()
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doCenterFloat, doFullFloat, doRectFloat)
import XMonad.Hooks.UrgencyHook
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
   [ [ className =? "Emacs"      --> doShift "β" ]
   , [ className =? "Chromium"   --> doShift "γ" ]
   , [ className =? "Firefox"    --> doShift "γ" ]
   , [ className =? "Vlc"        --> doShift "θ" ]
   , [ className =? "Skype"      --> doShift "ι" ]
   , [ isFullscreen              --> doFullFloat]
   , [ isDialog                  --> doCenterFloat] ]


myKeys :: [ (String, X()) ]
myKeys =  [ ("M-g"   , spawn "firefox")
          , ("M-v"   , spawn "vlc")
          , ("M-<Print>" , spawn "scrot")
          , ("M-S-<Print>" , spawn "sleep 0.2; scrot -s")
          , ("M-a"   , spBeckon "alsamixer")
          , ("M-h"   , spBeckon "htop")
          , ("M-S-e" , spBeckon "erl")
          , ("M-S-h" , spBeckon "ghci")
          , ("M-S-p" , spBeckon "ipython")
          , ("M-u"   , focusUrgent) ]


myLogHook :: Handle -> X ()
myLogHook xmproc = do
    dynamicLogWithPP $ xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "green" "" . shorten 100
        , ppHidden = xmobarColor "white" "" . noScratchPad
        , ppHiddenNoWindows = xmobarColor "gray" "" . noScratchPad
        }
        where
          noScratchPad ws = if ws == "NSP" then "" else ws


spBeckon :: String -> X ()
spBeckon = namedScratchpadAction scratchpads

centerScreen :: Rational -> ManageHook
centerScreen h = doRectFloat $ W.RationalRect ((1 - h)/2) ((1 - h)/2) h h

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "htop" "urxvt -e htop" (title =? "htop") (centerScreen 0.7)
              , NS "alsamixer" "urxvt -e alsamixer" (title =? "alsamixer") (centerScreen 0.7)
              , NS "erl" "urxvt -e erl" (title =? "erl") (centerScreen 0.7)
              , NS "ghci" "urxvt -e ghci" (title =? "ghci") (centerScreen 0.7)
              , NS "ipython" "urxvt -e ipython" (title =? "ipython") (centerScreen 0.7)
              ]


-- > xmonad $ withUrgencyHookC myUrgencyConfig $ defaultConfig

-- myUrgencyConfig :: UrgencyConfig
-- myUrgencyConfig = urgencyConfig { suppressWhen = OnScreen }

-- myUrgencyHintFgColor = "#000000"
-- myUrgencyHintBgColor = "#ff6565"


main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { manageHook        = manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads
        , layoutHook        = avoidStruts $ layoutHook defaultConfig
        , logHook           = myLogHook xmproc
        , terminal          = myTerminal
        , modMask           = myModMask
        , borderWidth       = myBorderWidth
        , focusFollowsMouse = myFocusFollowsMouse
        , XMonad.workspaces = myWorkspaces
	} `additionalKeysP` myKeys
