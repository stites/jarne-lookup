{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Main where

import Data.Text (Text)
import qualified Data.Text as T
import System.HIDAPI as Hid
import Control.Monad
import Control.Concurrent.MVar
import System.Posix.Signals (Handler, Handler(CatchOnce), installHandler, sigINT, sigTERM)
import System.IO
import Data.Aeson
import GHC.Generics
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU      -- from utf8-string
import System.Exit
import System.Posix.Signals
import Control.Concurrent
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Control.Exception as E


data Entry
  = Entry
  { outline :: Text
  , definition :: Text
  , dictionary :: Text
  , can_remove :: Bool
  } deriving (Show, Eq, Generic, FromJSON)


jarneVendorId = 1
jarneProductId = 19530
jarneSerialNumber = "fe:4c:2c:87:0d:db"
jarneUsagePage = 65329 -- 0xff31 is the right one to use!

main :: IO ()
main = do
  Hid.init
  ds <- Prelude.filter (\d -> Hid.usagePage d == jarneUsagePage) <$> Hid.enumerate (Just jarneProductId) (Just jarneVendorId)

  forM_ ds $ \d -> print d -- (productString d, productId d, vendorId d)
  di <- if Prelude.length ds == 1 then pure (Prelude.head ds) else do
            putStrLn "which device would you like to use?"
            nstr <- Prelude.getLine
            pure $ ds !! (Prelude.read nstr :: Int)
  d <- Hid.openDeviceInfo di

  repl d
  close d
  Hid.exit
 where
  repl :: Device -> IO ()
  repl d = do
    BS.putStr "lookup: "
    hFlush stdout
    BS.getLine >>= \case
      "bye" -> return ()
      word    -> do
        void $ Hid.write d ("lookup " <> word <> "\n")
        E.try (readJavelinEntries d T.empty 100) >>= \case
          Left e@(HIDAPIException _ _) -> print e
          Right Nothing -> putStrLn "no entries found" >> repl d
          Right (Just es) -> do
            forM_ es print
            repl d


  readJavelinEntries :: Device -> Text -> Int -> IO (Maybe [Entry])
  readJavelinEntries d part 0 = pure Nothing
  readJavelinEntries d part n = do
    out <- T.dropWhileEnd (\c -> c == '\NUL' || c == '\n') . T.pack . BSU.toString <$> Hid.read d 65
    if T.null out then pure Nothing
    else let nxt = T.strip (part <> out) in
      case (eitherDecode (BLU.fromString (T.unpack nxt)) :: Either String [Entry]) of
        Left _ -> readJavelinEntries d nxt (n - 1)
        Right es -> pure (Just es)
