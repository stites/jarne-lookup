{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Monad
import Data.Text (Text)
import System.HIDAPI (Device)
import System.IO
import System.Environment
import System.Exit
import System.Process
import Data.HashMap.Strict (traverseWithKey)

import Control.Exception         qualified as E
import Data.ByteString.UTF8      qualified as BS      -- from utf8-string
import Data.Text                 qualified as T
import Data.Text.Internal.Search qualified as T


main :: IO ()
main = T.indices "jarne" . T.toLower . T.pack <$> readProcess "bluetoothctl" ["devices", "Connected"] [] >>= \case
  [] -> hPutStrLn stdout "Jarne not found" >> exitWith (ExitFailure 1)
  _ -> getArgs >>= \case
    [] -> exitWith (ExitFailure 1)
    qs -> runHid $ \d ->
      sendLookup d (BS.fromString $ unwords qs) >> readEntriesDefault d >>= \case
        Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 1)
        Right es ->
          let assocs = groupByDefinition es in
          void $ flip traverseWithKey assocs $ \k es -> do
             putStrLn (T.unpack k <> ":")
             forM_ es $ \e ->
               putStrLn $ T.unpack $ "        " <> outline e
