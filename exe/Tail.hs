{-# language OverloadedStrings #-}
module Main where

import qualified Data.ByteString
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.ByteString.Streaming as B
import Streaming
import qualified Streaming.Prelude as S
import Control.Concurrent.QSem
import System.INotify
import System.IO (withFile,IOMode(ReadMode))
import System.Environment (getArgs)

tailing :: FilePath -> (B.ByteString IO () -> IO r) -> IO r
tailing filepath continuation = withINotify $ \i -> do
    sem <- newQSem 1
    addWatch i [Modify] filepath (\_ -> signalQSem sem)
    withFile filepath ReadMode (\h -> continuation (handleToStream sem h))
    where
    handleToStream sem h = B.concat . Streaming.repeats $ do
        lift (waitQSem sem)
        readWithoutClosing h
    -- Can't use B.fromHandle here because annoyingly it closes handle on EOF
    -- instead of just returning, and this causes problems on new appends.
    readWithoutClosing h = do
        c <- lift (Data.ByteString.hGetSome h defaultChunkSize)
        if Data.ByteString.null c
           then return ()
           else do B.chunk c
                   readWithoutClosing h
main :: IO ()
main = do
    filepath : _ <- getArgs
    tailing filepath B.stdout
