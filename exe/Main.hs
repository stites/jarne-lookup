{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
import System.HIDAPI as Hid
import Control.Monad
import System.IO
import Data.ByteString as BS


main :: IO ()
main = runHid lookupRepl

lookupRepl :: Device -> IO ()
lookupRepl d = do
  BS.putStr "lookup: "
  hFlush stdout
  BS.getLine >>= \case
    "bye" -> return ()
    word    -> sendLookup d word >> readEntriesDefault d >>= \case
      Left err -> putStrLn err
      Right es -> forM_ es print >> lookupRepl d
