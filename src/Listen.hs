{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Lib

import System.HIDAPI qualified as Hid
import Control.Monad
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.IORef
import Data.ByteString.UTF8 as BSU      -- from utf8-string
import System.Exit
import System.Posix.Signals
import Control.Concurrent
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Control.Exception as E


main :: IO ()
main = do
  -- Hid.init

  -- ds <- Prelude.filter (\d -> Hid.usagePage d == jarneUsagePage) <$> Hid.enumerate (Just jarneProductId) (Just jarneVendorId)

  -- forM_ ds $ \d -> print (Hid.productString d, Hid.productId d, Hid.vendorId d)
  -- di <- if Prelude.length ds == 1 then pure (Prelude.head ds) else do
  --           putStrLn "which device would you like to use?"
  --           nstr <- Prelude.getLine
  --           pure $ ds !! (Prelude.read nstr :: Int)
  -- d <- Hid.openDeviceInfo di

  -- tid <- myThreadId
  -- void $ installHandler keyboardSignal (Catch (close d >> cleanup d >> E.throwTo tid ExitSuccess)) Nothing

  ref <- newIORef T.empty
  runHid (\d -> forever act ref d >> pure ExitSuccess) >>= \case
  --   Left e@(HIDAPIException _ _) -> print e >> cleanup d
  --   Right () -> pure ()


 where
  act :: IORef Text -> Hid.Device -> IO ExitCode
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
