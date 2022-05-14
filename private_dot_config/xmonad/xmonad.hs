{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Bitwarden
import Control.Applicative (liftA2)
import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Monad (void, when)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON)
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Char (isPrint, isSpace)
import Data.Foldable (forM_)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import GHC.Generics (Generic)
import System.Environment (getEnv)
import XMonad
  ( Default (..),
    ExtensionClass (..),
    Full (..),
    LayoutClass,
    ManageHook,
    X,
    XConfig (..),
    className,
    composeAll,
    focus,
    io,
    mod4Mask,
    sendMessage,
    spawn,
    title,
    trace,
    windows,
    xK_Alt_L,
    xK_Tab,
    xK_q,
    xmonad,
    (-->),
    (<+>),
    (=?),
    (|||),
  )
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
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
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
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Loggers (logTitles)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
    scratchpadWorkspaceTag,
  )
import XMonad.Util.Paste (pasteString, sendKey)
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.WorkspaceCompare (filterOutWs)

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . addEwmhWorkspaceSort (return $ filterOutWs ["NSP"])
    . ewmh
    . withEasySB (statusBarProp "xmobar" (return myXmobarPP)) defToggleStrutsKey
    . docks
    $ myConfig

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
    `additionalKeysP` myKeymap

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
    NS "files" "kitty --title files ranger" (title =? "files") $ customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
  ]

-- Note: M1 is left alt, C-S-M1-M is the "hyper" key and C-S-M1 is the
-- "meh" key (special keys on my keyboard that emulate these
-- modifiers)
myKeymap =
  [ ("M-<Tab>", nextMatch History (return True)),
    ("M1-<Tab>", cycleRecentWindows [xK_Alt_L] xK_Tab xK_q),
    ("M-<Esc>", spawn "systemctl suspend"),
    -- Move focus to prev/next screen
    ("M-a", onPrevNeighbour def W.view),
    ("M-o", onNextNeighbour def W.view),
    -- Move focused window to prev/next screen
    ("M-S-a", onPrevNeighbour def W.shift),
    ("M-S-o", onNextNeighbour def W.shift),
    -- Click a window or drag a rectangle to screenshot it
    ("M-s s", unGrab *> spawn "scrot --freeze --line width=2,color=\"red\",mode=\"edge\" --select"),
    -- Take a screenshot of all monitors
    ("M-s d", unGrab *> spawn "scrot"),
    -- Take a screenshot of the current window
    ("M-s w", unGrab *> spawn "scrot --focused"),
    ("M-S-p", runOrRaisePrompt myPromptConfig),
    -- Directional window movement
    ("M-<Right>", sendMessage $ Go R),
    ("M-<Left>", sendMessage $ Go L),
    ("M-<Up>", sendMessage $ Go U),
    ("M-<Down>", sendMessage $ Go D),
    ("M-C-<Right>", sendMessage $ Swap R),
    ("M-C-<Left>", sendMessage $ Swap L),
    ("M-C-<Up>", sendMessage $ Swap U),
    ("M-C-<Down>", sendMessage $ Swap D),
    -- Prompts
    ( "M-p w",
      windowMultiPrompt
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
    ("M-p m", manPrompt myPromptConfig),
    ("M-p s", shellPrompt myPromptConfig),
    ("M-p x", xmonadPrompt myPromptConfig),
    ("M-p p f", bwLoginFillPrompt myPromptConfig),
    ("M-p p c", bwLoginCopyPrompt myPromptConfig),
    -- App launchers
    ("C-S-M1-e", spawn "myemacs"),
    ("C-S-M1-b", spawn "browser"),
    ("C-S-M1-M-b", spawn "private-browser"),
    ("C-S-M1-t", spawn $ terminal myConfig),
    -- Emacs launchers (mnemonic "E-macs")
    ("C-S-M1-M-e n", spawn "~/scripts/org-capture n"),
    ("C-S-M1-M-e t", spawn "~/scripts/org-capture t"),
    ("C-S-M1-M-e c", spawn "~/scripts/emacs-calc"),
    -- Bluetooth (mnemonic "C-onnection")
    ("C-S-M1-M-c o", spawn "bluetoothctl power on"),
    ("C-S-M1-M-c f", spawn "bluetoothctl power off"),
    ("C-S-M1-M-c m", spawn "~/scripts/connect-airpods.sh"),
    ("C-S-M1-M-c c", spawn "~/scripts/connect-airpods-no-mic.sh"),
    ("C-S-M1-M-c d", spawn "~/scripts/disconnect-airpods.sh"),
    -- Scratchpads
    ("C-S-M1-h", namedScratchpadAction myScratchpads "htop"),
    ("M-t", namedScratchpadAction myScratchpads "terminal"),
    ("M-f", namedScratchpadAction myScratchpads "files"),
    -- Workspaces
    ("M-w <Backspace>", removeWorkspace),
    ("M-w <Tab>", toggleWS),
    ("M-w e", moveTo Next emptyWS),
    ("M-w p", selectWorkspace myPromptConfig),
    -- Select a workspace to shift the current window to
    ("M-w s", withWorkspace myPromptConfig (windows . W.shift)),
    ("M-w r", renameWorkspace myPromptConfig),
    -- Move to/move window to next/prev workspace
    ("M-w j", nextWS),
    ("M-w k", prevWS),
    ("M-w S-j", shiftToNext >> nextWS),
    ("M-w S-k", shiftToPrev >> prevWS),
    ("M-w S-M-j", shiftToNext),
    ("M-w S-M-k", shiftToPrev),
    -- Move all windows to the next screen
    ("M-w a", withAll (\w -> focus w >> onNextNeighbour def W.shift)),
    -- Searching for things, either through the prompt or using the
    -- current selection (mnemonic "G-oogle")
    ("M-g g", promptSearchBrowser myPromptConfig "browser" (intelligent google)),
    ("M-g m", promptSearchBrowser myPromptConfig {defaultText = "!archman "} "browser" duckduckgo),
    ("M-g t", selectSearchBrowser "browser" (intelligent google)),
    ("M-g d", selectSearchBrowser "browser" dictionary),
    ("M-g h", selectSearchBrowser "browser" hoogle),
    ("M-g f", promptSearchBrowser myPromptConfig "browser" hoogle),
    -- Adjust monitor brightness
    ("M-<F11>", spawn "light -s sysfs/backlight/ddcci14 -U 1 && light -s sysfs/backlight/ddcci14 -O"),
    ("M-<F12>", spawn "light -s sysfs/backlight/ddcci14 -A 1 && light -s sysfs/backlight/ddcci14 -O")
  ]

myStartupHook :: X ()
myStartupHook = do
  -- Note: spawning the daemon using systemd instead of here does not
  -- include environment variables for emacs to access
  spawnOnce "emacs --daemon"
  spawnOnce "xbindkeys --poll-rc"
  spawnOnce "copyq"
  spawnOnce "redshift"
  checkKeymap myConfig myKeymap

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused],
      ppSort = return $ filterOutWs ["NSP"] -- Don't show scratchpads workspace
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
      title =? "Org Capture" --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.5),
      title =? "Emacs Quick Calc" --> doRectFloat (W.RationalRect 0.25 0.45 0.5 0.05),
      -- isDialog --> doFloat
      isFullscreen --> doFullFloat
    ]
