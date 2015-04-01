import XMonad
import XMonad.Actions.Search hiding (Query, images)
import XMonad.Actions.Submap
import XMonad.Actions.WorkspaceNames()
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doCenterFloat, doFullFloat, doRectFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.DecorationMadness
import XMonad.Layout.FixedColumn
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile
import XMonad.ManageHook()
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Ssh
import XMonad.Util.Dzen
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import Control.Applicative
import Data.Ratio ((%))

import Graphics.X11.Xinerama
import System.IO
import System.Posix.Unistd

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- TODO: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-TopicSpace.html
-- TODO: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-Dzen.html
-- TODO: https://github.com/supki/xmonad-screenshot

main :: IO ()
main = do
    dzenL <- spawnPipe myXmonadBar  -- dzenLeft
    _ <- spawnPipe myStatusBar      -- dzenRight
    -- host <- getHost
    xmonad $ myConfig dzenL

myXmonadBar :: String
myXmonadBar = dzenXmonad ++ dzenFont ++ dzenColours

myStatusBar :: String
myStatusBar = invokeConky ++ dzenStatus ++ dzenFont ++ dzenColours

dzenXmonad :: String
dzenXmonad = "dzen2 -p -ta l -w 400 -xs 1 "

dzenStatus :: String
dzenStatus = "dzen2 -p -ta r -w 820 -x 460 -xs 1 "

dzenFont :: String
dzenFont = "-fn 'inconsolata:size=8' "

dzenColours :: String
dzenColours = "-fg '#ffffff' -bg '#000000'"

invokeConky :: String
invokeConky = "conky -c ~/.conkyrc-xmonad | "

-- Bool informs us if a Windows key is present.
-- data Host = Desktop | Laptop Bool
--   deriving (Eq, Read, Show)

-- getHost :: IO Host
-- getHost = do
--   hostName <- nodeName `fmap` getSystemID
--   return $ case hostName of
--     "paradise" -> Laptop True
--     "" -> Desktop
--     _ -> Desktop

-- Used in order to properly calculate and fix the width of the status
-- bar across multiple screens.
type ScreenNum = Int

screenWidth :: ScreenNum -> IO Double
screenWidth s = do
    dsp <- openDisplay ""
    mss <- xineramaQueryScreens dsp
    return $ case mss of
        Nothing -> 0
        Just [] -> 0
        Just ss -> if s >= 0 && s < length ss -- Prevent bad index
            then fromIntegral . xsi_width $ ss !! s else 0

getScreens :: IO [ScreenNum]
getScreens = openDisplay "" >>= liftA2 (<*) f closeDisplay
    where f = fmap (zipWith const [0..]) . getScreenInfo

myConfig dzenL =
  withUrgencyHook NoUrgencyHook $ defaultConfig
        { manageHook = manageDocks
                       <+> myManageHook
                       <+> namedScratchpadManageHook scratchpads
        , layoutHook = myLayoutHook
        , logHook = myLogHook dzenL
        , terminal = myTerminal
        , modMask = myModMask
        , borderWidth = myBorderWidth
        , focusFollowsMouse = myFocusFollowsMouse
        , XMonad.workspaces = myWorkspaces
        } `additionalKeysP` myKeys

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ dzenPP
          { ppOutput = hPutStrLn h
          , ppCurrent = dzenColor "#f8f893" "" . wrap "[" "]" . noScratchPad
          , ppTitle = dzenColor "green" "" . pad. shorten 40
          , ppHidden = dzenColor "#5b605e" "" . pad . noScratchPad
          , ppHiddenNoWindows = const ""
          , ppLayout = dzenColor "#dcdccc" "#000000" . wrap " " ""
          , ppVisible = dzenColor "#f18c96" "" . wrap "[" "]" . noScratchPad
          , ppUrgent = dzenColor "black" "yellow" . pad
          --, ppUrgent = dzenColor "yellow" "red" . pad . dzenStrip
          , ppSep = " "
          , ppWsSep = ""
        }
        where
          noScratchPad ws = if ws == "NSP" then "" else ws

-- myUrgencyHook = withUrgencyHook dzenUrgencyHook
--     { args = ["-bg", "yellow", "-fg", "black" "-xs", "1"] }

myTerminal :: String
myTerminal = "urxvtc"

myBorderWidth :: Dimension
myBorderWidth = 2

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = map show [1 .. 9 :: Int]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- TODO: scratchpad for this also.
-- ircAction :: Host -> X ()
-- ircAction host = case host of
--   Laptop -> runInTerm "" "ssh <your screen server>"
--   Desktop -> runInTerm "" "tmux attach -t irc"

myManageHook :: ManageHook
myManageHook = composeAll . concat $
   [ [ className =? "Emacs" --> doShift "2" ]
   , [ className =? "Chromium" --> doShift "3" ]
   -- , [(className =? "Chromium" <&&> resource =? "Dialog") --> doFloat]
   , [ className =? "Firefox" --> doShift "4" ]
   , [ className =? "Evince" --> doShift "5" ]
   , [ className =? "Zathura" --> doShift "5" ]
   , [ className =? "Vlc" --> doShift "6" ]
   , [ className =? "Soulseekqt" --> doShift "7" ]
   , [ className =? "Transmission-gtk" --> doShift "7" ]
   , [ className =? "TeamSpeak 3" --> doShift "8" ]
   , [ className =? "Wync" --> doShift "8" ]
   , [ className =? "Pidgin" --> doShift "9" ]
   , [ className =? "Skype" --> doShift "9" ]
   , [ isFullscreen --> doFullFloat ]
   , [ isDialog --> doCenterFloat ] ]

-- Default tiling algorithm partitions the screen into two panes
basic :: Tall a
basic = Tall nmaster delta ratio
  where
    nmaster = 1 -- The default number of windows in the master pane
    delta = 3/100 -- Percent of screen to increment by when resizing panes
    ratio = 1/2 -- Default proportion of screen occupied by master pane

myLayoutHook = onWorkspace "9" im standardLayouts
  where
    standardLayouts = tall ||| wide ||| webdev ||| full ||| circle
    tall = named "tall" $ avoidStruts basic
    wide = named "wide" $ avoidStruts $ Mirror basic
    full = named "full" $ smartBorders $ noBorders Full
    circle = named "circle" $ avoidStruts circleSimpleDefaultResizable
    webdev = named "webdev" $ avoidStruts $ Mirror $ Tall nmaster delta ratio
      where
        nmaster = 1
        delta = 3/100
        ratio = 80/100

    -- Layout for IM buddy lists on either side of workspace
    im = named "IM" $ avoidStruts $
                      withIM 0.25 skype $
                      reflectHoriz $
                      withIM 0.25 pidgin (Grid ||| tall ||| circle)
      where
        pidgin = ClassName "Pidgin" `And` Role "buddy_list"
        skype = ClassName "Skype" `And`
                (Not (Title "Options")) `And`
                (Not (Role "Chats")) `And`
                (Not (Role "CallWindowForm"))

myKeys :: [ (String, X()) ]
myKeys =  [ ("M-u", focusUrgent)
          , ("M-S-u", clearUrgents)
            -- spawning
          , ("M-d", spawn "transmission-gtk")
          , ("M-g", spawn "firefox")
          , ("M-c", spawn "chromium")
          , ("M-m", spawn "soulseekqt")
          , ("M-i", spawn "pidgin")
          , ("M-r", spawn "evince")
          , ("M-s", spawn "skype")
          , ("M-t", spawn "teamspeak3")
          , ("M-v", spawn "vlc")
          , ("M-w", spawn "wync")
          , ("M-<Backspace>", spawn "mpc toggle")
          , ("<XF86AudioNext>", spawn "mpc next")
          , ("<XF86AudioPrev>", spawn "mpc prev")
          , ("C-M-r", spawn "mpc random")  -- toggle random play mode
          -- , ("M-<F1>", spawn "mpc pause; xscreensaver-command -lock")

          -- , ("M-S-s", spawn $
          --         case host of
          --           Thinkpad _ -> "systemctl suspend"
          --           _ -> "")

          , ("M-<Print>", spawn "scrot")
          , ("M-S-<Print>", spawn "sleep 0.2; scrot -s")
            -- scratchpads
          , ("M-a", spBeckon "alsamixer")
          , ("M-n", spBeckon "ncmpcpp")
          , ("M-S-t", spBeckon "htop")
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
                     , ((shiftMask, xK_h), method hackage)
                     , ((0, xK_h), method hoogle)
                     , ((0, xK_m), method maps)
                     , ((0, xK_w), method wikipedia)
                     , ((0, xK_y), method youtube)
                       -- custom searches
                     , ((0, xK_d), method discogs)
                     , ((0, xK_e), method egoogle)
                     , ((0, xK_b), method github)
                     , ((0, xK_i), method images)
                     , ((0, xK_t), method pb)
                     , ((0, xK_p), method pypi)
                     ]
                     where
                       discogs = searchEngine "discogs" "http://www.discogs.com/search/?q="
                       egoogle = searchEngine "egoogle" "https://encrypted.google.com/#q="
                       github = searchEngine "github" "https://github.com/search?q="
                       images = searchEngine "images" "http://www.google.com/search?hl=en&tbm=isch&q="
                       pb = searchEngine "pb" "http://tpb.unblock.re/search.php?q="
                       pypi = searchEngine "pypi" "https://pypi.python.org/pypi?%3Aaction=search&term="

-- Prompt search: get input from the user via a prompt, run the search
-- in the browser and automatically switch to the web workspace.
myPromptSearch :: SearchEngine -> X ()
myPromptSearch (SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ \s ->
      search "chromium" site s >> viewWeb

-- Select search: do a search based on the X selection
mySelectSearch :: SearchEngine -> X ()
mySelectSearch eng = selectSearch eng >> viewWeb

viewWeb :: X ()
viewWeb = windows (W.view "3")

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig { fgColor = "#000000" , bgColor = "#ff6565" }

spBeckon :: String -> X ()
spBeckon = namedScratchpadAction scratchpads

centerScreen :: Rational -> ManageHook
centerScreen h = doRectFloat $ W.RationalRect ((1 - h)/2) ((1 - h)/2) h h

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "alsamixer" "urxvtc -e alsamixer" (title =? "alsamixer") (centerScreen 0.7)
              , NS "erl" "urxvtc -e erl" (title =? "erl") (centerScreen 0.7)
              , NS "ghci" "urxvtc -e ghci" (title =? "ghci") (centerScreen 0.7)
              , NS "htop" "urxvtc -e htop" (title =? "htop") (centerScreen 0.7)
              , NS "ipython" "urxvtc -e ipython" (title =? "ipython") (centerScreen 0.7)
              , NS "ncmpcpp" "urxvtc -e ncmpcpp" (title =? "ncmpcpp") (centerScreen 0.7) ]

--spawnEmacs :: X ()
--spawnEmacs = spawn ("emacsclient -c")
