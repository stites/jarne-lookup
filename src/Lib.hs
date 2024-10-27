
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fno-warn-unused-top-binds #-}

module Lib where

import System.HIDAPI qualified as Hid
import System.HIDAPI (Device, HIDAPIException(..))
import Control.Monad
import Control.Lens as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Internal.Search qualified as T
import Data.Aeson
import GHC.Generics
import GHC.Word
import GHC.Natural
import Data.ByteString.UTF8 as BSU      -- from utf8-string
import Data.ByteString as BS
import System.Exit (ExitCode(..))
import System.Posix.Signals
import Control.Concurrent
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Control.Exception as E


data Entry
  = Entry
  { outline :: Text
  , definition :: Text
  , dictionary :: Maybe Text
  , can_remove :: Maybe Bool
  } deriving (Show, Eq, Generic, FromJSON)

data Keeb
  = Keeb
  { _vendorId :: Word16
  , _productId :: Word16
  , _serialNumber :: String
  , _usagePage :: Word16
  } deriving (Show, Eq)
$(makeLenses ''Keeb)

jarne :: Keeb
jarne = Keeb
  { _vendorId = 1
  , _productId = 19530
  , _serialNumber = "fe:4c:2c:87:0d:db"
  , _usagePage = 65329 -- 0xff31 is the right one to use!
  }

devices :: IO [Hid.DeviceInfo]
devices = do
  ds <- Prelude.filter (\d -> Hid.usagePage d == jarne ^. usagePage) <$> Hid.enumerate (Just $ jarne ^. productId ) (Just $ jarne ^. vendorId)
  forM_ ds $ \d -> print (Hid.productString d, Hid.productId d, Hid.vendorId d)
  pure ds

firstDevice :: IO (Maybe Hid.Device)
firstDevice = do
  ds <- devices
  putStrLn "selecting first device"
  E.try (Hid.openDeviceInfo (Prelude.head ds)) >>= \case
    Left (_ :: E.SomeException) -> pure Nothing
    Right d -> pure (Just d)

runHid :: (Device -> IO ()) -> IO ()
runHid act = do
  Hid.init
  ds <- devices

  di <- if Prelude.length ds == 1 then pure (Prelude.head ds) else do
            putStrLn "which device would you like to use?"
            nstr <- Prelude.getLine
            pure $ ds !! (Prelude.read nstr :: Int)
  putStrLn "selected:"
  putStrLn $ "  " <> show di

  d <- Hid.openDeviceInfo di

  tid <- myThreadId
  void $ installHandler keyboardSignal (Catch (cleanup d >> E.throwTo tid ExitSuccess)) Nothing

  E.try (act d) >>= \case
    Left e@(HIDAPIException _ _) -> print e >> cleanup d
    Right () -> cleanup d

 where

  cleanup :: Device -> IO ()
  cleanup d = do
    Hid.close d -- hid_error cleans up device handles, so we have to avoid the double-free
    Hid.exit
    putStrLn "closed the hid handle"
-----------------------------------------------------------------------------------------------
-- * Javelin repl
-----------------------------------------------------------------------------------------------

sendLookup :: Device -> BS.ByteString -> IO ()
sendLookup d word = void $ Hid.write d ("lookup " <> word <> "\n")

readEntries :: Device -> Natural -> IO (Either String [Entry])
readEntries = go T.empty
  where
    go :: Text -> Device -> Natural -> IO (Either String [Entry])
    go    _ _ 0 = pure $ Left "none found"
    go part d n = do
      out <- T.dropWhileEnd (\c -> c == '\NUL' || c == '\n') . T.pack . BSU.toString <$> Hid.read d 65
      if T.null out then
        pure $ Left "none found"
      else do
        let nxt = T.strip (part <> out)
        case T.indices "ERR Invalid command." nxt  of
          [] -> case (eitherDecode (BLU.fromString (T.unpack nxt)) :: Either String [Entry]) of
            Left _ -> go nxt d (n - 1)
            Right es -> pure (Right es)
          i:_ -> do
            putStrLn "found error"
            let stopat = T.indices "]" nxt
            if Prelude.null stopat
            then pure $ Left $ T.unpack (T.drop i nxt)
            else case (eitherDecode (BLU.fromString (T.unpack (T.take i nxt))) :: Either String [Entry]) of
              Left err -> pure (Left err)
              Right es -> pure (Right es)

readEntriesDefault :: Device -> IO (Either String [Entry])
readEntriesDefault d = readEntries d 100
