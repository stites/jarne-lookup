{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (runHid, sendLookup, readEntriesDefault, groupByDefinition, Entry(outline))

import Control.Monad (void, forM_)
import System.IO (hPutStrLn, stdout, stderr)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Process (readProcess)
import Data.HashMap.Strict (traverseWithKey)

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
          void $ flip traverseWithKey assocs $ \k es' -> do
             putStrLn (T.unpack k <> ":")
             forM_ es' $ \e ->
               putStrLn $ T.unpack $ "        " <> outline e
