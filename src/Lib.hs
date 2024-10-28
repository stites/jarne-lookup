{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fno-warn-unused-top-binds #-}
module Lib where

import System.HIDAPI qualified as Hid
import System.HIDAPI (Device, DeviceInfo, HIDAPIException(..))
import System.Environment
import System.IO
import Control.Monad
import Control.Lens as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Internal.Search qualified as T
import Data.HashMap.Strict (fromListWith, HashMap)
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

groupByDefinition :: [Entry] -> HashMap Text [Entry]
groupByDefinition = fromListWith (++) . fmap (\e -> (definition e, [e]))

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
  -- forM_ ds $ \d -> print (Hid.productString d, Hid.productId d, Hid.vendorId d)
  pure ds

firstDevice :: IO (Maybe Hid.Device)
firstDevice = do
  ds <- devices
  -- putStrLn "selecting first device"
  E.try (Hid.openDeviceInfo (Prelude.head ds)) >>= \case
    Left (_ :: E.SomeException) -> pure Nothing
    Right d -> pure (Just d)


runHid_ :: ([DeviceInfo] -> IO DeviceInfo) -> (Device -> IO ()) -> IO ()
runHid_ selectionPolicy act = do
  Hid.init
  ds <- devices
  di <- selectionPolicy ds
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
    -- putStrLn "closed the hid handle"

runHid :: (Device -> IO ()) -> IO ()
runHid = runHid_ (pure . Prelude.head)

runHidInteractive :: (Device -> IO ()) -> IO ()
runHidInteractive = runHid_ $ \ds -> do
  di <- if Prelude.length ds == 1 then pure (Prelude.head ds) else do
            putStrLn "which device would you like to use?"
            nstr <- Prelude.getLine
            pure $ ds !! (Prelude.read nstr :: Int)
  putStrLn "selected:"
  putStrLn $ "  " <> show di
  pure di

----------------------------------------------------------------------------------------------
-- * Javelin repl
-----------------------------------------------------------------------------------------------

sendLookup :: Device -> BS.ByteString -> IO ()
sendLookup d word = do
  let cmd = "lookup " <> word <> "\n"
  -- Prelude.putStr ">>> "
  -- print $ cmd
  void $ Hid.write d cmd

readEntries :: Device -> Natural -> IO (Either String [Entry])
readEntries d = go T.empty
  where
    go
      :: Text      -- ^ accumulated text from the hid device buffer
      -> Natural   -- ^ a safety bound to make sure we don't recurse indefinitely.
                   --   in practice this is unnecceary as Hid.read blocks

      -> IO (Either String [Entry]) -- ^ return either an error message or a list of entries

    go    _ 0 = pure $ Left "none found"

    go part n = do
      -- read from the hid device, but drop any garbage at the end of the string.
      out <- T.dropWhileEnd (\c -> c == '\NUL' || c == '\n') . T.pack . BSU.toString <$> Hid.read d 65

      -- if we have an empty string, we are done
      if T.null out then pure $ Left "none found"

      -- o/w get everything we've aquired so far
      else let nxt = T.strip (part <> out) in

        -- apparently we can / often get back both a valid output _and_ an ERR.
        -- I'm guessing this is because of a mismatch of the ByteString encoding
        -- I've chosen.
        case T.indices "ERR Invalid command. Use \"help\" for a list of commands" nxt  of

          -- happy path: no error!
          [] ->
            case (eitherDecode (BLU.fromString (T.unpack nxt)) :: Either String [Entry]) of
              Left _ -> go nxt (n - 1)    -- if the parse fails, don't worry and try again.
              Right es -> pure (Right es) -- otherwise, we are all good!

          -- sad path: we need to break up the string to see if a valid entry was also generated
          i:_ -> do
            hPutStrLn stderr "found error"
            let stopat = T.indices "]" nxt -- good json outputs are always arrays

            -- if we didn't find an array, then it is all error
            if Prelude.null stopat then pure $ Left $ T.unpack (T.drop i nxt)

            -- otherwise, take everything up to the ']' and decode, this time really error if we fail
            else case (eitherDecode (BLU.fromString (T.unpack (T.take i nxt))) :: Either String [Entry]) of
              Left err -> pure (Left err)
              Right es -> do
                hPutStrLn stderr $ T.unpack nxt
                pure (Right es)

readEntriesDefault :: Device -> IO (Either String [Entry])
readEntriesDefault d = readEntries d 100
