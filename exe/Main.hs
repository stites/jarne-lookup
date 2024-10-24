{-# LANGUAGE OverloadedStrings #-}

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

-- sendLookup :: Device -> BS.ByteString -> IO ()
-- sendLookup d word = void $ Hid.write d ("lookup " <> word <> "\n")

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


-- listenService :: IORef (Maybe [Entry]) -> IO ()
-- listenService rez = do
--   ref <- newIORef T.empty
--   runHid (forever act ref)
--  where
--   act :: IORef Text -> Hid.Device -> IO ()
--   act r d = do
--     out <- T.dropWhileEnd (\c -> c == '\NUL' || c == '\n') . T.pack . BSU.toString <$> Hid.read d 65
--     if T.null out then
--        writeIORef rez Nothing
--     else do
--       f <- T.strip . (<> out) <$> readIORef r
--       case (eitherDecode (BLU.fromString (T.unpack $ f)) :: Either String [Entry]) of
--         Left _ -> writeIORef r f
--         Right es -> do
--           writeIORef rez (Just es)
--           writeIORef r T.empty
--           forM_ es $ \e -> print e


main :: IO ()
main = runHid lookupRepl

lookupRepl :: Device -> IO ()
lookupRepl d = do
  BS.putStr "lookup: "
  hFlush stdout
  BS.getLine >>= \case
    "bye" -> return ()
    word    -> sendLookup d word >> lookupRepl d
