{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import System.HIDAPI as Hid
import Control.Monad
import System.IO
import Data.ByteString as BS

main :: IO ()
main = runHid lookupLoop
 where
  lookupLoop :: Device -> IO ()
  lookupLoop d = do
    BS.putStr "lookup: "
    hFlush stdout
    BS.getLine >>= \case
      "bye" -> return ()
      word    -> do
        void $ Hid.write d ("lookup " <> word <> "\n")
        lookupLoop d
