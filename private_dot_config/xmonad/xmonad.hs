{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Bitwarden
import Control.Applicative (liftA2)
import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Monad (void, when)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON)
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Char (isPrint, isSpace)
import Data.Foldable (forM_)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import GHC.Generics (Generic)
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import System.IO (hClose, openFile)
import System.Posix (executeFile, mkstemp)
import XMonad
import XMonad.Actions.CycleWS
  ( Direction1D (..),
    emptyWS,
    moveTo,
    nextWS,
    prevWS,
    shiftToNext,
    shiftToPrev,
    toggleWS,
  )
import XMonad.Actions.CycleWindows (cycleRecentWindows)
import XMonad.Actions.DynamicWorkspaces (removeWorkspace, renameWorkspace, selectWorkspace, withWorkspace)
import XMonad.Actions.GroupNavigation (Direction (History), historyHook, nextMatch)
import XMonad.Actions.PhysicalScreens (onNextNeighbour, onPrevNeighbour)
import XMonad.Actions.Search (dictionary, duckduckgo, google, hoogle, intelligent, promptSearchBrowser, selectSearchBrowser)
import XMonad.Actions.WithAll (withAll)
import XMonad.Config.Desktop ()
import XMonad.Hooks.EwmhDesktops (addEwmhWorkspaceSort, ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers
  ( doFullFloat,
    doRectFloat,
    isFullscreen,
  )
import XMonad.Hooks.StatusBar
  ( defToggleStrutsKey,
    statusBarProp,
    withEasySB,
  )
import XMonad.Hooks.StatusBar.PP
  ( PP
      ( ppCurrent,
        ppExtras,
        ppHidden,
        ppHiddenNoWindows,
        ppOrder,
        ppSep,
        ppSort,
        ppTitleSanitize,
        ppUrgent
      ),
    shorten,
    wrap,
    xmobarBorder,
    xmobarColor,
    xmobarRaw,
    xmobarStrip,
  )
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHookExclude)
import XMonad.Layout.CenteredIfSingle (centeredIfSingle)
import XMonad.Layout.Circle (Circle (Circle))
import XMonad.Layout.Gaps (Direction2D (..), gaps)
import XMonad.Layout.Grid (Grid (..))
import XMonad.Layout.Magnifier (magnifiercz)
import XMonad.Layout.Master (mastered)
import XMonad.Layout.MultiDishes
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.OneBig
import XMonad.Layout.Renamed (Rename (PrependWords), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing (smartSpacing, smartSpacingWithEdge, spacingWithEdge)
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation (Navigate (..), windowNavigation)
import XMonad.Prompt
  ( XPConfig (..),
    XPPosition (..),
    XPrompt (..),
    getNextCompletion,
    mkComplFunFromList,
    mkXPrompt,
    setBorderColor,
    vimLikeXPKeymap',
  )
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window
  ( WindowPrompt (..),
    allWindows,
    windowMultiPrompt,
    wsWindows,
  )
import XMonad.Prompt.XMonad (xmonadPrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap, mkNamedKeymap)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Loggers (logTitles)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
    scratchpadWorkspaceTag,
  )
import XMonad.Util.Paste (pasteString, sendKey)
import XMonad.Util.Run (hPutStr, runProcessWithInput)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.WorkspaceCompare (filterOutWs)
import Bitwarden (bwPasswordFillPrompt)

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . addEwmhWorkspaceSort (return $ filterOutWs ["NSP"])
    . ewmh
    . withEasySB (statusBarProp "xmobar" (return myXmobarPP)) defToggleStrutsKey
    . addMyDefaultKeys ((mod4Mask, xK_F1), showKeybindingsAction) addMyKeymap
    $ myConfig
  where
    addMyDefaultKeys k ks = addDescrKeys' k (\l -> myDefaultKeys l ^++^ ks l)
    showKeybindingsAction =
      addName "Show Keybindings"
        . zenity "XMonad Keybindings"
        . unlines
        . showKm

myConfig =
  def
    { terminal = "kitty",
      modMask = mod4Mask, -- Rebind mod to the super key
      focusedBorderColor = "#88C0D0",
      normalBorderColor = "#2E3440",
      focusFollowsMouse = False,
      startupHook = myStartupHook,
      manageHook = myManageHook <+> manageDocks <+> namedScratchpadManageHook myScratchpads,
      layoutHook = smartBorders $ avoidStruts myLayoutConfig,
      logHook = historyHook >> workspaceHistoryHookExclude [scratchpadWorkspaceTag],
      workspaces = ["1", "2"]
    }

myLayoutConfig =
  windowNavigation $
    renamed [PrependWords "Centered"] (gaps [(U, 6), (D, 6)] $ centeredIfSingle 0.8 Full)
      ||| spacingWithEdge 3 (TwoPane (3 / 100) (1 / 2))
      ||| Full
      ||| magnifiercz 1.2 Circle

myPromptConfig :: XPConfig
myPromptConfig =
  def
    { font = "xft:jetbrains mono:size=12",
      height = 35,
      position = CenteredAt 0.5 0.5,
      bgColor = "#2E3440",
      fgColor = "#ECEFF4",
      bgHLight = "#88C0D0",
      borderColor = "#88C0D0",
      promptKeymap = vimLikeXPKeymap' (setBorderColor "#5E81AC") ("[n] " ++) (filter isPrint) isSpace,
      defaultPrompter = ("[i] " ++),
      searchPredicate = fuzzyMatch,
      sorter = fuzzySort
    }

myScratchpads =
  [ NS "htop" "kitty --title htop htop" (title =? "htop") $ customFloating $ W.RationalRect (1 / 12) (1 / 12) (10 / 12) (10 / 12),
    NS "terminal" "kitty --title terminal" (title =? "terminal") $ customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3),
    NS "files" "kitty --title files ranger" (title =? "files") $ customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3),
    NS
      "calc"
      "emacsclient --create-frame --frame-parameters \"'(name . \\\"Emacs Calc\\\")\" --eval \"(full-calc)\""
      (title =? "Emacs Calc")
      $ customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
  ]

addMyKeymap c = (subtitle "Custom Keys" :) $ mkNamedKeymap c myKeymap

-- Note: M1 is left alt, C-S-M1-M is the "hyper" key and C-S-M1 is the
-- "meh" key (special keys on my keyboard that emulate these
-- modifiers)
myKeymap =
  [ ("M-<Tab>", addName "Previous window" $ nextMatch History (return True)),
    ("M1-<Tab>", addName "Cycle windows in MRU order" $ cycleRecentWindows [xK_Alt_L] xK_Tab xK_q),
    ("M-<Esc>", addName "Suspend" $ spawn "systemctl suspend"),
    ("C-S-M1-r", addName "Clipboard history" $ spawn "copyq toggle"),
    -- Move focus to prev/next screen
    ("M-a", addName "Focus previous screen" $ onPrevNeighbour def W.view),
    ("M-o", addName "Focus previous screen" $ onNextNeighbour def W.view),
    -- Move focused window to prev/next screen
    ("M-S-a", addName "Move window to previous screen" $ onPrevNeighbour def W.shift),
    ("M-S-o", addName "Move window to next screen" $ onNextNeighbour def W.shift),
    -- Click a window or drag a rectangle to screenshot it
    ( "M-s s",
      addName "Screenshot window or rectangle" $
        unGrab *> spawn "scrot --freeze --line width=2,color=\"red\",mode=\"edge\" --select"
    ),
    -- Take a screenshot of all monitors
    ("M-s d", addName "Screenshot all monitors" $ unGrab *> spawn "scrot"),
    -- Take a screenshot of the current window
    ("M-s w", addName "Screenshot current window" $ unGrab *> spawn "scrot --focused"),
    ("M-S-p", addName "Run command or raise window of command" $ runOrRaisePrompt myPromptConfig),
    -- Directional window movement
    ("M-<Right>", addName "Move to rightward window" $ sendMessage $ Go R),
    ("M-<Left>", addName "Move to leftward window" $ sendMessage $ Go L),
    ("M-<Up>", addName "Move to upward window" $ sendMessage $ Go U),
    ("M-<Down>", addName "Move to downward window" $ sendMessage $ Go D),
    ("M-C-<Right>", addName "Swap with rightward window" $ sendMessage $ Swap R),
    ("M-C-<Left>", addName "Swap with leftward window" $ sendMessage $ Swap L),
    ("M-C-<Up>", addName "Swap with upward window" $ sendMessage $ Swap U),
    ("M-C-<Down>", addName "Swap with downward window" $ sendMessage $ Swap D),
    -- Prompts
    ( "M-p w",
      addName
        "Select a window in the current workspace, or hit ` and bring a window from another workspace"
        $ windowMultiPrompt
          myPromptConfig {autoComplete = Just 500000}
          [ (BringToMaster, wsWindows),
            ( Bring, -- Select among all windows not in the current workspace
              liftA2
                (M.\\)
                -- allWindows adds " [n]" to the end of the window names, we take that out so the key comparison works
                (M.mapKeys (\s -> take (length s - 4) s) <$> allWindows)
                wsWindows
            )
          ]
    ),
    ("M-p m", addName "Select a man page" $ manPrompt myPromptConfig),
    ("M-p s", addName "Run a shell command" $ shellPrompt myPromptConfig),
    ("M-p x", addName "Run an xmonad command" $ xmonadPrompt myPromptConfig),
    ( "M-p p f",
      addName "Select then type a username and password from bitwarden" $
        bwLoginFillPrompt myPromptConfig
    ),
    ( "M-p p p",
      addName "Select then type a password from bitwarden" $
        bwPasswordFillPrompt myPromptConfig
    ),
    ( "M-p p c",
      addName "Select then copy a username and password from bitwarden" $
        bwLoginCopyPrompt myPromptConfig
    ),
    -- App launchers
    ("C-S-M1-e", addName "Launch emacs" $ spawn "myemacs --create-frame"),
    ("C-S-M1-b", addName "New browser window" $ spawn "browser"),
    ("C-S-M1-M-b", addName "New private browser window" $ spawn "private-browser"),
    ("C-S-M1-t", addName "Launch terminal" $ spawn $ terminal myConfig),
    -- Emacs launchers (mnemonic "E-macs")
    ("C-S-M1-M-e n", addName "Capture a note" $ spawn "~/scripts/org-capture n"),
    ("C-S-M1-M-e t", addName "Capture a todo item" $ spawn "~/scripts/org-capture t"),
    ( "C-S-M1-M-e c",
      addName "Run a quick calculation and copy to clipboard" $
        spawn "~/scripts/emacs-calc"
    ),
    -- Bluetooth (mnemonic "C-onnection")
    ("C-S-M1-M-c o", addName "Bluetooth on" $ spawn "bluetoothctl power on"),
    ("C-S-M1-M-c f", addName "Bluetooth off" $ spawn "bluetoothctl power off"),
    ( "C-S-M1-M-c m",
      addName "Connect airpods and turn on mic" $
        spawn "~/scripts/connect-airpods.sh"
    ),
    ("C-S-M1-M-c c", addName "Connect airpods" $ spawn "~/scripts/connect-airpods-no-mic.sh"),
    ("C-S-M1-M-c d", addName "Disconnect airpods" $ spawn "~/scripts/disconnect-airpods.sh"),
    -- Scratchpads
    ("C-S-M1-h", addName "Toggle htop" $ namedScratchpadAction myScratchpads "htop"),
    ("M-t", addName "Toggle terminal" $ namedScratchpadAction myScratchpads "terminal"),
    ("M-f", addName "Toggle file manager" $ namedScratchpadAction myScratchpads "files"),
    ("C-S-M1-c", addName "Toggle calculator" $ namedScratchpadAction myScratchpads "calc"),
    -- Workspaces
    ("M-w <Backspace>", addName "Delete workspace" removeWorkspace),
    ("M-w <Tab>", addName "Go to previously visited workspace" toggleWS),
    ("M-w e", addName "Go to an empty workspace" $ moveTo Next emptyWS),
    ("M-w p", addName "Select/create a workspace by name" $ selectWorkspace myPromptConfig),
    ("M-w t", addName "Push floating to tiled" $ withFocused $ windows . W.sink),
    -- Select a workspace to shift the current window to
    ( "M-w s",
      addName "Move window to a workspace" $
        withWorkspace myPromptConfig (windows . W.shift)
    ),
    ("M-w r", addName "Rename workspace" $ renameWorkspace myPromptConfig),
    -- Move to/move window to next/prev workspace
    ("M-w j", addName "Move to next workspace" nextWS),
    ("M-w k", addName "Move to previous workspace" prevWS),
    ("M-w S-j", addName "Move window to next workspace and follow" $ shiftToNext >> nextWS),
    ("M-w S-k", addName "Move window to previous workspace and follow" $ shiftToPrev >> prevWS),
    ("M-w S-M-j", addName "Move window to next workspace" shiftToNext),
    ("M-w S-M-k", addName "Move window to previous workspace" shiftToPrev),
    -- Move all windows to the next screen
    ( "M-w a",
      addName "Move all windows to the next screen" $
        withAll (\w -> focus w >> onNextNeighbour def W.shift)
    ),
    -- Searching for things, either through the prompt or using the
    -- current selection (mnemonic "G-oogle")
    ( "M-g g",
      addName "Google search" $
        promptSearchBrowser myPromptConfig "browser" (intelligent google)
    ),
    ( "M-g m",
      addName "Search Arch man pages" $
        promptSearchBrowser myPromptConfig {defaultText = "!archman "} "browser" duckduckgo
    ),
    ( "M-g t",
      addName "Google the current selection" $
        selectSearchBrowser "browser" (intelligent google)
    ),
    ( "M-g d",
      addName "Lookup the current selection in the dictionary" $
        selectSearchBrowser "browser" dictionary
    ),
    ("M-g h", addName "Hoogle the current selection" $ selectSearchBrowser "browser" hoogle),
    ("M-g f", addName "Hoogle search" $ promptSearchBrowser myPromptConfig "browser" hoogle),
    ("M-<F9>", addName "Redraw and cycle the wallpaper" $ spawn "variety --next"),
    ("M-<F10>", addName "Reload the monitor layout" $ spawn "autorandr --force --change"),
    -- Adjust monitor brightness
    ( "M-<F11>",
      addName "Decrease monitor brightness" $
        spawn "light -s sysfs/backlight/ddcci14 -U 1 && light -s sysfs/backlight/ddcci14 -O"
    ),
    ( "M-<F12>",
      addName "Increase monitor brightness" $
        spawn "light -s sysfs/backlight/ddcci14 -A 1 && light -s sysfs/backlight/ddcci14 -O"
    )
  ]

myStartupHook :: X ()
myStartupHook = do
  -- Note: spawning the daemon using systemd instead of here does not
  -- include environment variables for emacs to access
  spawnOnce "emacs --daemon"
  spawnOnce "xbindkeys --poll-rc"
  spawnOnce "copyq"
  spawnOnce "redshift"
  checkKeymap myConfig (map (second $ const (return () :: X ())) myKeymap)

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused],
      ppSort = return $ filterOutWs [scratchpadWorkspaceTag] -- Don't show scratchpads workspace
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#B48EAD" ""
    blue = xmobarColor "#5E81AC" ""
    white = xmobarColor "#ECEFF4" ""
    yellow = xmobarColor "#EBCB8B" ""
    red = xmobarColor "#BF616A" ""
    lowWhite = xmobarColor "#E5E9F0" ""

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "copyq" --> doRectFloat (W.RationalRect 0.7 0.25 0.25 0.5), -- Right/middle of screen, quarter width, half height
      className =? "Zenity" --> doFloat,
      title =? "Org Capture" --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.5),
      title =? "Emacs Quick Calc" --> doRectFloat (W.RationalRect 0.25 0.45 0.5 0.05),
      -- isDialog --> doFloat
      isFullscreen --> doFullFloat
    ]

-- | Use @zenity@ to show information to the user.
zenity :: MonadIO m => String -> String -> m ()
zenity title msg = void . xfork $ do
  (t, h) <- mkstemp "/tmp/zenity"
  hPutStr h msg
  hClose h
  executeFile
    "zenity"
    True
    [ "--title",
      title,
      "--text-info",
      "--font",
      "JetBrains Mono",
      "--width",
      "800",
      "--height",
      "1000",
      "--filename",
      t
    ]
    Nothing

-- | Custom name default keys
myDefaultKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myDefaultKeys conf@XConfig {XMonad.modMask = modm} =
  [ subtitle "launching and killing programs",
    ((modm .|. shiftMask, xK_c), addName "Close the focused window" kill), -- %! Close the focused window
    subtitle "changing layouts",
    ((modm, xK_space), sendMessage' NextLayout), -- %! Rotate through the available layout algorithms
    ((modm .|. shiftMask, xK_space), addName "Reset the layout" $ setLayout $ XMonad.layoutHook conf), -- %!  Reset the layouts on the current workspace to default
    separator,
    ((modm, xK_n), addName "Refresh" refresh), -- %! Resize viewed windows to the correct size
    subtitle "move focus up or down the window stack",
    ((modm, xK_j), addName "Focus down" $ windows W.focusDown), -- %! Move focus to the next window
    ((modm, xK_k), addName "Focus up" $ windows W.focusUp), -- %! Move focus to the previous window
    ((modm, xK_m), addName "Focus the master" $ windows W.focusMaster), -- %! Move focus to the master window
    subtitle "modifying the window order",
    ((modm, xK_Return), addName "Swap with the master" $ windows W.swapMaster), -- %! Swap the focused window and the master window
    ((modm .|. shiftMask, xK_j), addName "Swap down" $ windows W.swapDown), -- %! Swap the focused window with the next window
    ((modm .|. shiftMask, xK_k), addName "Swap up" $ windows W.swapUp), -- %! Swap the focused window with the previous window
    subtitle "resizing the master/slave ratio",
    ((modm, xK_h), sendMessage' Shrink), -- %! Shrink the master area
    ((modm, xK_l), sendMessage' Expand), -- %! Expand the master area
    subtitle "change the number of windows in the master area",
    ((modm, xK_comma), sendMessage' (IncMasterN 1)), -- %! Increment the number of windows in the master area
    ((modm, xK_period), sendMessage' (IncMasterN (-1))), -- %! Deincrement the number of windows in the master area
    subtitle "quit, or restart",
    ((modm .|. shiftMask, xK_q), addName "Quit" $ io exitSuccess), -- %! Quit xmonad
    ((modm, xK_q), addName "Restart" $ spawn "xmonad --recompile && xmonad --restart") -- %! Restart xmonad
  ]
