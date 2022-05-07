{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | Library for using bitwarden from xmonad
module Bitwarden where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON)
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Foldable (forM_)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Generics (Generic)
import System.Environment (getEnv)
import XMonad
  ( ExtensionClass (..),
    X,
    io,
    spawn,
    trace,
    xK_Tab,
  )
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
import XMonad.Prompt.XMonad (xmonadPrompt)
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Paste (pasteString, sendKey)
import XMonad.Util.Run (runProcessWithInput)

newtype BWPrompt = BWPrompt String

instance XPrompt BWPrompt where
  showXPrompt (BWPrompt p) = p ++ ": "
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

data BWItem = BWItem
  { name :: String,
    id :: String,
    login :: Maybe BWLogin
  }
  deriving (Generic, Show)

instance FromJSON BWItem

data BWLogin = BWLogin
  { uris :: [BWUri],
    username :: Maybe String,
    password :: Maybe String
  }
  deriving (Generic, Show)

instance FromJSON BWLogin

data BWUri = BWUri
  { uri :: Maybe String
  }
  deriving (Generic, Show)

instance FromJSON BWUri

type BWItems = M.Map String BWItem

mkBwPassPrompt :: String -> (BWItem -> X ()) -> XPConfig -> X ()
mkBwPassPrompt promptLabel passwordFunction xpconfig = do
  bwItems <- io getBwItems
  mkXPrompt
    (BWPrompt promptLabel)
    xpconfig
    ( mkComplFunFromList
        xpconfig
          { -- Modify the completion string before passing it to the
            -- search predicate so that the uri is also searched. This
            -- allows for a website's login domain to pasted into the
            -- prompt
            searchPredicate = \typed compl -> searchPredicate xpconfig typed (appendBwItemUris bwItems compl)
          }
        $ getBwItemsComplList bwItems
    )
    (\s -> passwordFunction $ bwItems M.! s)
  where
    appendBwItemUris bwItems c =
      c
        ++ maybe
          ""
          (foldr (\u b -> maybe "" (++ (" " ++ b)) (uri u)) "" . uris)
          (login (bwItems M.! c))

bwLoginFillPrompt :: XPConfig -> X ()
bwLoginFillPrompt =
  mkBwPassPrompt
    "Login to fill"
    ( \item -> do
        forM_ (login item >>= username) pasteString
        sendKey 0 xK_Tab
        forM_ (login item >>= password) pasteString
    )

bwLoginCopyPrompt :: XPConfig -> X ()
bwLoginCopyPrompt =
  mkBwPassPrompt
    "Login to copy"
    ( \item -> do
        forM_ (login item >>= password) stringToClipboard
        io $ threadDelay 200000
        forM_ (login item >>= username) stringToClipboard
    )

getEnvVar :: String -> IO (Either IOError String)
getEnvVar = E.try . getEnv

escape :: String -> String
escape [] = ""
escape (x : xs)
  | isSpecialChar x = '\\' : x : escape xs
  | otherwise = x : escape xs

isSpecialChar :: Char -> Bool
isSpecialChar = flip elem "$"

stringToClipboard s =
  spawn $ "echo -n \"" ++ escape s ++ "\" | xclip -in -selection clipboard"

getBwItemsComplList :: BWItems -> [String]
getBwItemsComplList = M.keys

getBwItems :: IO BWItems
getBwItems = do
  -- bwSession <- getEnvVar "BW_SESSION"
  -- case bwSession of
  --   Left ie -> trace $ "Error getting BW_SESSION: " ++ show ie
  -- Right _ -> trace "BW_SESSION retrieved"
  pwJsonString <- runProcessWithInput "fish" ["-c", "bw list items"] ""
  -- trace pwJsonString
  let bwItems = eitherDecode (B.fromString pwJsonString) :: Either String [BWItem]
  case bwItems of
    Left s -> do
      trace s
      return M.empty
    Right items -> return $ M.fromList $ map (\item -> (getItemId item, item)) items
  where
    getItemId item = name item ++ maybe "" (" " ++) (login item >>= username)
