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
import XMonad.Actions.CycleWindows (cycleRecentWindows)
import XMonad.Actions.DynamicWorkspaces (removeWorkspace, renameWorkspace, selectWorkspace, withWorkspace)
import XMonad.Actions.GroupNavigation (Direction (History), historyHook, nextMatch)
import XMonad.Actions.Search (dictionary, duckduckgo, google, hoogle, intelligent, promptSearchBrowser, selectSearchBrowser)
import XMonad.Config.Desktop ()
import XMonad.Hooks.EwmhDesktops (addEwmhWorkspaceSort, ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docks)
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
  )
import XMonad.Util.Paste (pasteString, sendKey)
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Util.SpawnOnce (spawnOnce)
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
      logHook = historyHook,
      workspaces = ["1"]
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

-- Note: M1 is left alt
myKeymap =
  [ ("M-<Tab>", nextMatch History (return True)),
    ("M1-<Tab>", cycleRecentWindows [xK_Alt_L] xK_Tab xK_q),
    ("M-S-p", runOrRaisePrompt myPromptConfig),
    ("M-<Right>", sendMessage $ Go R),
    ("M-<Left>", sendMessage $ Go L),
    ("M-<Up>", sendMessage $ Go U),
    ("M-<Down>", sendMessage $ Go D),
    ("M-C-<Right>", sendMessage $ Swap R),
    ("M-C-<Left>", sendMessage $ Swap L),
    ("M-C-<Up>", sendMessage $ Swap U),
    ("M-C-<Down>", sendMessage $ Swap D),
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
    ("C-S-M1-e", spawn "myemacs"),
    ("C-S-M1-b", spawn "browser"),
    ("C-S-M1-t", spawn $ terminal myConfig),
    ("C-S-M1-h", namedScratchpadAction myScratchpads "htop"),
    ("C-S-M1-M-e n", spawn "~/scripts/org-capture n"),
    ("C-S-M1-M-e t", spawn "~/scripts/org-capture t"),
    ("M-t", namedScratchpadAction myScratchpads "terminal"),
    ("M-f", namedScratchpadAction myScratchpads "files"),
    ("M-S-<Backspace>", removeWorkspace),
    ("M-S-v", selectWorkspace myPromptConfig),
    ("M-n", withWorkspace myPromptConfig (windows . W.shift)),
    ("M-S-n", renameWorkspace myPromptConfig),
    ("M-g g", promptSearchBrowser myPromptConfig "browser" (intelligent google)),
    ("M-g m", promptSearchBrowser myPromptConfig {defaultText = "!archman "} "browser" duckduckgo),
    ("M-g t", selectSearchBrowser "browser" (intelligent google)),
    ("M-g d", selectSearchBrowser "browser" dictionary),
    ("M-g h", selectSearchBrowser "browser" hoogle),
    ("M-g f", promptSearchBrowser myPromptConfig "browser" hoogle)
  ]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xbindkeys"
  spawnOnce "copyq"
  -- Note: spawning the daemon using systemd instead of here does not
  -- include environment variables for emacs to access
  spawnOnce "emacs --daemon"
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
      -- isDialog --> doFloat
      isFullscreen --> doFullFloat
    ]
