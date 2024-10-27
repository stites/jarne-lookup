{-# language OverloadedStrings #-}
module Main where

import Lib
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BSU      -- from utf8-string
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import Data.Char
import Data.Aeson
import Data.Either
import Data.Function ((&))
import Streaming as S
import Streaming.Prelude qualified as SP
import Streaming.ByteString (ByteStream)
import Streaming.ByteString qualified as S
import Control.Concurrent.QSem
import System.INotify
import System.IO (withFile,IOMode(ReadMode), Handle, hFlush, stdout)
import System.Environment (getArgs)

import Data.ByteString.Streaming.Aeson
import Control.Concurrent

type EntryStream m x = Stream (Of Results) m x

newtype Results = Results (Either String [Entry])
  deriving Show
instance Semigroup Results where
instance Monoid Results where
  mappend (Results (Left l) ) (Results (Left r)) = Results $ Left (l   <> r)
  mappend (Results (Right l)) (Results (Left r)) = Results $ Right l
  mappend (Results (Left l) ) (Results (Right r)) =Results $  Right (r)
  mappend (Results (Right l)) (Results (Right r)) =Results $  Right (l <> r)

tailing :: (EntryStream IO () -> IO r) -> FilePath -> IO r
tailing k filepath = withINotify $ \i -> do
  sem <- newQSem 1
  void $ addWatch i [Modify] (BSU.fromString filepath) (\_ -> signalQSem sem)
  withFile filepath ReadMode (k . handleToRepl sem)

  where
    handleToRepl :: QSem -> Handle -> EntryStream IO ()
    -- handleToRepl sem h = S.concat . S.repeats $ do
    handleToRepl sem h =
      -- lift (waitQSem sem)
      -- readWithoutClosing h
      S.hGetContents h & S.repeats & S.concat & parseAsOptJson

    -- readWithoutClosing :: Handle -> ByteStream IO ()
    -- readWithoutClosing h = do
    --   c <- lift (BS.hGetSome h defaultChunkSize)
    --   if BS.null c
    --   then return ()
    --   else S.chunk c >> readWithoutClosing h

    syncToTVar :: Handle -> ByteStream IO ()
    syncToTVar h = undefined

    parseAsOptJson :: ByteStream IO () -> ByteStream IO (Results)
    parseAsOptJson b = do
      bs <- S.toStrict b
      pure $ parseEntries bs

    parseEntries x = case (eitherDecode x :: Either String [Entry]) of
          Left _ -> Left (BLU.toString x)
          Right es -> Right es



main :: IO ()
main = do
  f <- head <$> getArgs
  tailing (SP.print) f
-- main = SP.stdoutLn $ SP.map ("lookup " <>) $ SP.take 2 SP.stdinLn

-- main :: IO ()
-- main = getArgs >>= \case
--   [] -> putStrLn "help does not exist"
--   rst -> do
--     ref <- newIORef Nothing
--     forkIO $ listenService ref
--     sleep1
--     runHid $ \d -> do
--       sendLookup d (BSU.fromString $ unwords rst)
--       sleep1
--       readIORef ref >>= \case
--         Nothing -> putStrLn "no entries found"
--         Just ez -> forM_ ez print


-- listenService :: IORef (Maybe [Entry]) -> IO ()
-- listenService rez = do
--   ref <- newIORef T.empty
--   runHid (forever act ref)
--  where
--   act :: IORef Text -> Hid.Device -> IO ()
--   act r d = do
--     out <- T.dropWhileEnd (\c -> c == '\NUL' || c == '\n') . T.pack . BSU.toString <$> Hid.read d 65
--     if T.null out then
--        writeIORef rez Nothing
--     else do
--       f <- T.strip . (<> out) <$> readIORef r
--       case (eitherDecode (BLU.fromString (T.unpack $ f)) :: Either String [Entry]) of
--         Left _ -> writeIORef r f
--         Right es -> do
--           writeIORef rez (Just es)
--           writeIORef r T.empty
--           forM_ es $ \e -> print e
