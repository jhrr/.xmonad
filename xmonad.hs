import XMonad
import XMonad.ManageHook()
import XMonad.Actions.WorkspaceNames()
import XMonad.Actions.Submap
import XMonad.Actions.Search hiding (Query, images)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doCenterFloat, doFullFloat, doRectFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import System.IO

import XMonad.StackSet as W
import qualified Data.Map as M


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
   , [ isFullscreen --> doFullFloat ]
   , [ isDialog --> doCenterFloat ] ]


myKeys :: [ (String, X()) ]
myKeys =  [ ("M-g", spawn "firefox")
          , ("M-v", spawn "vlc")
          , ("M-<Print>", spawn "scrot")
          , ("M-S-<Print>", spawn "sleep 0.2; scrot -s")
          , ("M-a", spBeckon "alsamixer")
          , ("M-h", spBeckon "htop")
          , ("M-S-e", spBeckon "erl")
          , ("M-S-h", spBeckon "ghci")
          , ("M-S-p", spBeckon "ipython")
          , ("M-u", focusUrgent)
          , ("M-/", submap . mySearchMap $ myPromptSearch)
          , ("M-C-/", submap . mySearchMap $ mySelectSearch) ]


mySearchMap method = M.fromList $
                     [ ((0, xK_g), method google)
                     , ((0, xK_w), method wikipedia)
                     , ((0, xK_h), method hoogle)
                     , ((shiftMask, xK_h), method hackage)
                     , ((0, xK_m), method maps)
                     , ((0, xK_y), method youtube)
                       -- custom searches
                     , ((0, xK_i), method images)
                     , ((0, xK_p), method pypi)
                     ]

images = searchEngine "images" "http://www.google.com/search?hl=en&tbm=isch&q="
pypi = searchEngine "pypi" "https://pypi.python.org/pypi?%3Aaction=search&term="

-- Prompt search: get input from the user via a prompt, then run the
--   search in firefox and automatically switch to the web workspace
myPromptSearch :: SearchEngine -> X ()
myPromptSearch (SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ \s ->
      (search "firefox" site s >> viewWeb)

-- Select search: do a search based on the X selection
mySelectSearch eng = selectSearch eng >> viewWeb

viewWeb = windows (W.view "γ")

myXPConfig = defaultXPConfig
    { fgColor = "#000000"
    , bgColor = "#ff6565"
    }


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
scratchpads = [ NS "htop" (myTerminal ++ "-e htop") (title =? "htop") (centerScreen 0.7)
              , NS "alsamixer" (myTerminal ++ "-e alsamixer") (title =? "alsamixer") (centerScreen 0.7)
              , NS "erl" (myTerminal ++ "-e erl") (title =? "erl") (centerScreen 0.7)
              , NS "ghci" (myTerminal ++ "-e ghci") (title =? "ghci") (centerScreen 0.7)
              , NS "ipython" (myTerminal ++ "-e ipython") (title =? "ipython") (centerScreen 0.7) ]


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
