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
-- for dzen xinerama, one bar on each, we would need -xs 1 and -xs 2 in each

main :: IO ()
main = do
    dzenL <- spawnPipe myXmonadBar
    dzenR <- spawnPipe myStatusBar
    -- host <- getHost
    xmonad $ myConfig dzenL

myXmonadBar = "dzen2 -p -ta l -w 400 -fn " ++ dzenFont ++ dzenColours
myStatusBar = "conky -c ~/.conkyrc | dzen2 -p -ta r -w 820 -x 460 -fn " ++ dzenFont ++ dzenColours
dzenColours = "-fg '#ffffff' -bg '#000000'"
--dzenFont = "'inconsolata-hellenic:size=8' "
dzenFont = "'-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*' "


myConfig dzenL =
  myUrgencyHook $ defaultConfig
        { manageHook = manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = myLogHook dzenL
        , terminal = myTerminal
        , modMask = myModMask

        -- , modMask = if host == Laptop False
        --             then modMask def
        --             else mod4Mask

        , borderWidth = myBorderWidth
        , focusFollowsMouse = myFocusFollowsMouse
        , XMonad.workspaces = myWorkspaces
        } `additionalKeysP` myKeys

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ dzenPP
          { ppOutput = hPutStrLn h
          , ppTitle = dzenColor "#ffff00" "" . pad. shorten 40
          , ppHidden = dzenColor "#909090" "" . pad . noScratchPad
          , ppHiddenNoWindows = dzenColor "#606060" "" . pad . noScratchPad
          , ppWsSep = ""
          -- , ppUrgent = dzenColor "yellow" "red" . pad .dzenStrip
        }
        where
          noScratchPad ws = if ws == "NSP" then "" else ws

myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-bg", "yellow", "-fg", "black"] }

-- Bool informs us if the machine has a Windows key
-- data Host = Laptop Bool | Desktop Bool | Other
--   deriving (Eq, Read, Show)

-- getHost :: IO Host
-- getHost = do
--   hostName <- nodeName `fmap` getSystemID
--   return $ case hostName of
--     "paradise" -> Thinkpad True
--     "" -> Desktop True
--     _ -> Other

myTerminal :: String
myTerminal = "urxvt"

myBorderWidth :: Dimension
myBorderWidth = 2

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = ["α", "β" ,"γ", "δ", "ε", "ζ", "η", "θ", "ι"]


myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- ircAction :: Host -> X ()
-- ircAction host = case host of
--   Laptop -> runInTerm "" "ssh <your screen server>"
--   Desktop -> runInTerm "" "tmux attach -t irc"

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
          , ("M-S-u", clearUrgents)
            -- spawning
          , ("M-g", spawn "firefox")
          , ("M-c", spawn "chromium")
          , ("M-v", spawn "vlc")
          , ("M-<Backspace>", spawn "mpc toggle")
          , ("<XF86AudioNext>", spawn "mpc next")
          , ("<XF86AudioPrev>", spawn "mpc prev")
          , ("C-M-r", spawn "mpc random")  -- toggle random play mode
          -- , ("M-<F1>", spawn "mpc pause; xscreensaver-command -lock")

          -- , ("M-S-s", spawn $
          --         case host of
          --           Laptop _ -> "pm-suspend"
          --           _ -> "systemctl suspend")

          , ("M-<Print>", spawn "scrot")
          , ("M-S-<Print>", spawn "sleep 0.2; scrot -s")
            -- scratchpads
          , ("M-a", spBeckon "alsamixer")
          , ("M-n", spBeckon "ncmpcpp")
          , ("M-t", spBeckon "htop")
          , ("M-S-e", spBeckon "erl")
          , ("M-S-h", spBeckon "ghci")
          , ("M-S-p", spBeckon "ipython")
            -- screens
          -- , ("M-s", nextScreen)
          -- , ("M-w", swapNextScreen)
          -- , ("M-e", shiftNextScreen)
            -- searches
          , ("M-p s", sshPrompt myXPConfig)
          , ("M-/", submap . mySearchMap $ myPromptSearch)
          , ("M-S-/", submap . mySearchMap $ mySelectSearch) ]

          ++

          [ (otherModMasks ++ "M-" ++ [key], action tag)
          | (tag, key)  <- zip myWorkspaces "123456789"
          , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- was W.greedyView
                                       , ("S-", windows . W.shift)] ]


          -- mod-[1..],       Switch to workspace N
          -- mod-shift-[1..], Move client to workspace N
          -- mod-ctrl-[1..],  Switch to workspace N on other screen

          -- [ (m ++ "M-" ++ [k], f i)
          --     | (i, k) <- zip (XMonad.workspaces conf) "123456789-=[]\\"
          --     , (f, m) <- [ (windows . W.shift, "S-")
          --                 , (goto', "")
          --                 , (\ws -> nextScreen >> (goto' $ ws), "C-")
          --                 ]]



mySearchMap :: (SearchEngine -> a) -> M.Map (KeyMask, KeySym) a
mySearchMap method = M.fromList
                     [ ((0, xK_g), method google)
                     , ((0, xK_w), method wikipedia)
                     , ((0, xK_h), method hoogle)
                     , ((shiftMask, xK_h), method hackage)
                     , ((0, xK_m), method maps)
                     , ((0, xK_y), method youtube)
                       -- custom searches
                     , ((0, xK_d), method discogs)
                     , ((0, xK_b), method github)
                     , ((0, xK_i), method images)
                     , ((0, xK_t), method pb)
                     , ((0, xK_p), method pypi)
                     ]
                     where
                       discogs = searchEngine "discogs" "http://www.discogs.com/search/?q="
                       github = searchEngine "github" "https://github.com/search?q="
                       images = searchEngine "images" "http://www.google.com/search?hl=en&tbm=isch&q="
                       pb = searchEngine "pb" "http://bayproxy.me/search/"
                       pypi = searchEngine "pypi" "https://pypi.python.org/pypi?%3Aaction=search&term="


-- Prompt search: get input from the user via a prompt, then run the
--   search in firefox and automatically switch to the web workspace
myPromptSearch :: SearchEngine -> X ()
myPromptSearch (SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ \s ->
      search "firefox" site s >> viewWeb

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

spBeckon :: String -> X ()
spBeckon = namedScratchpadAction scratchpads

centerScreen :: Rational -> ManageHook
centerScreen h = doRectFloat $ W.RationalRect ((1 - h)/2) ((1 - h)/2) h h

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "alsamixer" "urxvt -e alsamixer" (title =? "alsamixer") (centerScreen 0.7)
              , NS "erl" "urxvt -e erl" (title =? "erl") (centerScreen 0.7)
              , NS "ghci" "urxvt -e ghci" (title =? "ghci") (centerScreen 0.7)
              , NS "htop" "urxvt -e htop" (title =? "htop") (centerScreen 0.7)
              , NS "ipython" "urxvt -e ipython" (title =? "ipython") (centerScreen 0.7)
              , NS "ncmpcpp" "urxvt -e ncmpcpp" (title =? "ncmpcpp") (centerScreen 0.7) ]



-- TODO: TopicSpaces: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-TopicSpace.html
