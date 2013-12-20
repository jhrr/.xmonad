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


myTerminal :: [Char]
myTerminal = "urxvt"

myBorderWidth :: Dimension
myBorderWidth = 2

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [[Char]]
myWorkspaces = ["α", "β" ,"γ", "δ", "ε", "ζ", "η", "θ", "ι"]

myFocusFollowsMouse :: Bool
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
          -- TODO: org-750 pad/topicspace?
          , ("M-u", focusUrgent)
          , ("M-/", submap . mySearchMap $ myPromptSearch)
          , ("M-S-/", submap . mySearchMap $ mySelectSearch) ]


mySearchMap :: (SearchEngine -> a) -> M.Map (KeyMask, KeySym) a
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
                     where
                       images = searchEngine "images" "http://www.google.com/search?hl=en&tbm=isch&q="
                       pypi = searchEngine "pypi" "https://pypi.python.org/pypi?%3Aaction=search&term="

-- Prompt search: get input from the user via a prompt, then run the
--   search in firefox and automatically switch to the web workspace
myPromptSearch :: SearchEngine -> X ()
myPromptSearch (SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ \s ->
      (search "firefox" site s >> viewWeb)

-- Select search: do a search based on the X selection
mySelectSearch :: SearchEngine -> X ()
mySelectSearch eng = selectSearch eng >> viewWeb

viewWeb :: X ()
viewWeb = windows (W.view "γ")

myXPConfig :: XPConfig
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
scratchpads = [ NS "htop" "urxvt -e htop" (title =? "htop") (centerScreen 0.7)
              , NS "alsamixer" "urxvt -e alsamixer" (title =? "alsamixer") (centerScreen 0.7)
              , NS "erl" "urxvt -e erl" (title =? "erl") (centerScreen 0.7)
              , NS "ghci" "urxvt -e ghci" (title =? "ghci") (centerScreen 0.7)
              , NS "ipython" "urxvt -e ipython" (title =? "ipython") (centerScreen 0.7) ]


-- > xmonad $ withUrgencyHookC myUrgencyConfig $ defaultConfig

-- myUrgencyConfig :: UrgencyConfig
-- myUrgencyConfig = urgencyConfig { suppressWhen = OnScreen }
-- myUrgencyHintFgColor = "#000000"
-- myUrgencyHintBgColor = "#ff6565"


-- TODO: TopicSpaces: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-TopicSpace.html


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
