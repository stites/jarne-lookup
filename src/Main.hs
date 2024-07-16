{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.HIDAPI as Hid
import Control.Monad
import Control.Concurrent.MVar
import System.Posix.Signals (Handler, Handler(CatchOnce), installHandler, sigINT, sigTERM)
import System.IO
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU      -- from utf8-string

jarneVendorId = 1
jarneProductId = 19530
jarneSerialNumber = "fe:4c:2c:87:0d:db"
jarneUsagePage = 65329 -- 0xff31 is the right one to use!

main :: IO ()
main = do

  Hid.init

  -- -- ds <- Hid.enumerateAll
  ds <- Prelude.filter (\d -> Hid.usagePage d == jarneUsagePage) <$> Hid.enumerate (Just jarneProductId) (Just jarneVendorId)

  forM_ ds $ \d -> print d -- (productString d, productId d, vendorId d)
  di <- if Prelude.length ds == 1 then pure (Prelude.head ds) else do
            putStrLn "which device would you like to use?"
            nstr <- Prelude.getLine
            pure $ ds !! (Prelude.read nstr :: Int)
  d <- Hid.openDeviceInfo di
  repl d
  --   Hid.write d "lookup test" >>= print
  close d

  -- forM_ ds $ \di -> do
  --   print di
  --   d <- Hid.openDeviceInfo di
  --   Hid.write d "lookup test" >>= print
  --   close d
  Hid.exit
 where
  -- loop :: MVar () -> IO () -> IO ()
  -- loop v act = act >> tryTakeMVar v >>= \case
  --   Just _ -> pure ()
  --   Nothing -> void $ loop v act
  repl :: Device -> IO ()
  repl d = do
    BS.putStr "lookup: "
    hFlush stdout
    BS.getLine >>= \case
      "bye" -> return ()
      word    -> do
        void $ Hid.write d ("lookup " <> word <> "\n")
        repl d
