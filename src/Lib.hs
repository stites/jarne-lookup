
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Lib
  ( Entry(..)
  , Hid.init
  , Hid.DeviceInfo(..)
  , Hid.Device
  , devices
  , firstDevice
  , runHid
  ) where

import System.HIDAPI qualified as Hid
import System.HIDAPI (Device, HIDAPIException(..))
import Control.Monad
import Control.Lens as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Aeson
import GHC.Generics
import GHC.Word
import Data.IORef
import Data.ByteString.UTF8 as BSU      -- from utf8-string
import System.Exit (ExitCode(..))
import System.Posix.Signals
import Control.Concurrent
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Control.Exception as E


data Entry
  = Entry
  { outline :: Text
  , definition :: Text
  , dictionary :: Text
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
    Left (e :: E.SomeException) -> pure Nothing
    Right d -> pure (Just d)

runHid :: (Device -> IO ExitCode) -> IO ()
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
    Right ExitSuccess -> cleanup d
    Right (ExitFailure _) -> cleanup d

 where

  cleanup :: Device -> IO ()
  cleanup d = do
    Hid.close d -- hid_error cleans up device handles, so we have to avoid the double-free
    Hid.exit
    putStrLn "closed the hid handle"
