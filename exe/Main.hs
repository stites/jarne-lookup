{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
import System.HIDAPI as Hid
import Control.Monad
import System.IO
import System.Environment
import Data.ByteString as BS
import Control.Concurrent
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Data.ByteString.UTF8 as BSU      -- from utf8-string
import System.Exit
import System.Posix.Signals
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Control.Exception as E
import Control.Concurrent

-- sleep1 :: IO ()
-- sleep1 = threadDelay 1000000 --sleep for a million microseconds, or one second

-- main :: IO ()
-- main = getArgs >>= \case
--   [] -> putStrLn "help does not exist"
--   rst -> do
--     ref <- newIORef Nothing
--     forkIO $ listenService ref
--     sleep1
--     runHid $ \d -> do
--       sendLookup d (BSU.fromString $ unwords rst)
--       sleep1
--       readIORef ref >>= \case
--         Nothing -> putStrLn "no entries found"
--         Just ez -> forM_ ez print



main :: IO ()
main =  runHid lookupRepl

lookupRepl :: Device -> IO ()
lookupRepl d = do
  BS.putStr "lookup: "
  hFlush stdout
  BS.getLine >>= \case
    "bye" -> return ()
    word    -> sendLookup d word >> readEntriesDefault d >>= \case
      Left err -> putStrLn err
      Right es -> forM_ es print >> lookupRepl d
