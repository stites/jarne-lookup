{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Lib

import System.HIDAPI qualified as Hid
import Control.Lens
import Control.Monad
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


main :: IO ()
main = do
  tid <- myThreadId
  void $ installHandler keyboardSignal (Catch (E.throwTo tid ExitSuccess)) Nothing

  Hid.init

  ds <- Prelude.filter (\d -> Hid.usagePage d == jarne ^. usagePage) <$> Hid.enumerate (Just $ jarne ^. productId) (Just $ jarne ^. vendorId)

  forM_ ds $ \d -> print (Hid.productString d, Hid.productId d, Hid.vendorId d)
  di <- if Prelude.length ds == 1 then pure (Prelude.head ds) else do
            putStrLn "which device would you like to use?"
            nstr <- Prelude.getLine
            pure $ ds !! (Prelude.read nstr :: Int)
  d <- Hid.openDeviceInfo di

  ref <- newIORef T.empty
  runHid (\d -> forever act ref d)


 where
  act :: IORef Text -> Hid.Device -> IO ()
  act r d = do
    out <- T.dropWhileEnd (\c -> c == '\NUL' || c == '\n') . T.pack . BSU.toString <$> Hid.read d 65
    if T.null out then
       putStrLn "no entries found"
    else do
      str <- readIORef r
      let f = T.strip $ str <> out
      case (eitherDecode (BLU.fromString (T.unpack $ f)) :: Either String [Entry]) of
        Left _ -> writeIORef r f
        Right es -> do
          writeIORef r T.empty
          forM_ es $ \e -> print e

  -- cleanup :: Device -> IO ()
  -- cleanup d = do
  --   -- close d -- hid_error cleans up device handles, so we have to avoid the double-free
  --   -- Hid.exit
  --   putStrLn "closed the hid handle"

-- main :: IO ()
-- main = do
--   ref <- newIORef T.empty
--   runHid (\d -> forever act ref d)

--  where
--   act :: IORef Text -> Hid.Device -> IO ()
--   act r d = do
--     out <- T.dropWhileEnd (\c -> c == '\NUL' || c == '\n') . T.pack . BSU.toString <$> Hid.read d 65
--     if T.null out then
--        putStrLn "no entries found"
--     else readEntries d 100 >>= \case
--       Nothing -> putStrLn "no entries found"
--       Just es -> forM_ es print
