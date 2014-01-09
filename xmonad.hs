import XMonad
import XMonad.Actions.Search hiding (Query, images)
import XMonad.Actions.Submap
import XMonad.Actions.WorkspaceNames()
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doCenterFloat, doFullFloat, doRectFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.ManageHook()
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Ssh
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run

import System.IO
import System.Posix.Unistd()

import qualified XMonad.StackSet as W
import qualified Data.Map as M


-- topic layer -- abstractions over common work patterns
-- application layer -- control over individual programs
-- shortcuts/bindings vs. prompts


main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    -- host <- getHost
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { manageHook = manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = myLogHook xmproc
        , terminal = myTerminal
        , modMask = myModMask
        , borderWidth = myBorderWidth
        , focusFollowsMouse = myFocusFollowsMouse
        , XMonad.workspaces = myWorkspaces
	} `additionalKeysP` myKeys


-- Bool informs us if the machine has a Windows key
-- data Host = Laptop Bool | Desktop Bool | Other
--   deriving (Eq, Read, Show)

-- getHost :: IO Host
-- getHost = do
--   hostName <- nodeName `fmap` getSystemID
--   return $ case hostName of
--     "paradise" -> Laptop True
--     "" -> Desktop True
--     _ -> Other


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

-- ircAction :: Host -> X ()
-- ircAction host = case host of
--   Laptop -> runInTerm "" "ssh <your screen server>"
--   Desktop -> runInTerm "" "screen -dRR"

myManageHook :: ManageHook
myManageHook = composeAll . concat $
   [ [ className =? "Emacs" --> doShift "β" ]
   , [ className =? "Chromium" --> doShift "γ" ]
   , [ className =? "Firefox" --> doShift "γ" ]
   , [ className =? "Zathura" --> doShift "δ" ]
   , [ className =? "Vlc" --> doShift "θ" ]
   , [ className =? "Skype" --> doShift "ι" ]
   , [ className =? "Transmission-gtk" --> doShift "ι" ]
   , [ isFullscreen --> doFullFloat ]
   , [ isDialog --> doCenterFloat ] ]


myKeys :: [ (String, X()) ]
myKeys =  [ ("M-u", focusUrgent)
            -- spawning
          , ("M-g", spawn "firefox")
          , ("M-v", spawn "vlc")
          , ("M-<Print>", spawn "scrot")
          , ("M-S-<Print>", spawn "sleep 0.2; scrot -s")
            -- scratchpads
          , ("M-a", spBeckon "alsamixer")
          , ("M-t", spBeckon "htop")
          , ("M-S-e", spBeckon "erl")
          , ("M-S-h", spBeckon "ghci")
          , ("M-S-p", spBeckon "ipython")
            -- searches
          , ("M-p s", sshPrompt myXPConfig)
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
                     , ((0, xK_t), method pb)
                     , ((0, xK_p), method pypi)
                     ]
                     where
                       images = searchEngine "images" "http://www.google.com/search?hl=en&tbm=isch&q="
                       pb = searchEngine "pb" "http://bayproxy.me/search/"
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
        , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
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


-- TODO: TopicSpaces: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-TopicSpace.html
