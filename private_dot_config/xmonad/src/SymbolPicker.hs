-- | Provides xmonad prompts for searching through and inserting
-- unicode symbols given data files listing the symbols.
module SymbolPicker (mkSymbolPrompt, mkSymbolPrompt') where

import Control.Arrow
import qualified Control.Exception as E
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map as M
import Paths_xmonad_dev
import System.FilePath
import System.IO
import XMonad
import XMonad.Prompt

type Symbol = Char

type SymbolDesc = String

type Symbols = M.Map Symbol SymbolDesc

newtype SymbolPrompt = SymbolPrompt String

instance XPrompt SymbolPrompt where
  showXPrompt (SymbolPrompt p) = p ++ ": "
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

-- | Make a prompt to select a symbol from the given symbol file
-- located in the @data@ folder. The prompt will show history while
-- nothing is typed for easily tabbing through frequent
-- symbols. Typing will search through the descriptions of the
-- symbols. Selecting will type the symbol into the current window
-- using xdotool.
mkSymbolPrompt ::
  -- | Symbol file name
  String ->
  -- | Prompt label (should be unique in order for history to work
  -- properly)
  String ->
  XPConfig ->
  X ()
mkSymbolPrompt symbolsFile promptLabel xpconfig = do
  file <- liftIO $ getDataFileName ("data" </> symbolsFile)
  mkSymbolPrompt' file promptLabel xpconfig

-- | Version of @mkSymbolPrompt@ that expects the full path to the
-- symbols file.
mkSymbolPrompt' :: FilePath -> String -> XPConfig -> X ()
mkSymbolPrompt' symbolsFilePath promptLabel xpconfig = do
  symbols <- liftIO $ getSymbols symbolsFilePath
  cacheDirPath <- asks (cacheDir . directories)
  mkXPrompt
    (SymbolPrompt promptLabel)
    xpconfig
    (compl symbols cacheDirPath)
    (spawn . ("xdotool type " ++) . (: []) . head)
  where
    -- Show symbol history for this prompt when nothing has been
    -- typed, otherwise search through the symbols
    compl :: Symbols -> FilePath -> ComplFunction
    compl symbols cacheDirPath typed
      | null typed =
        historyCompletionP cacheDirPath (promptLabel `isPrefixOf`) typed
      | otherwise =
        mkComplFunFromList
          xpconfig
          (map (\(s, ss) -> s : ' ' : ss) $ M.toList symbols)
          typed

    -- Copied from XMonad.Prompt, but modified to take the cache dir
    -- path as an argument
    historyCompletionP cacheDirPath p s =
      let toComplList = deleteConsecutive . filter (isInfixOf s) . M.foldr (++) []
       in toComplList . M.filterWithKey (const . p) <$> readHistory cacheDirPath

getSymbols :: FilePath -> IO Symbols
getSymbols =
  runKleisli $
    Kleisli readFile >>^ lines >>> foldMap (parseSymbolLine >>> uncurry M.singleton)
  where
    -- Symbol lines contain a single UTF-8 character followed by a
    -- space and then a description
    parseSymbolLine :: String -> (Symbol, SymbolDesc)
    parseSymbolLine = head &&& tail . tail
