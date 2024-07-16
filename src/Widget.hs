{-# LANGUAGE OverloadedStrings, OverloadedLabels, FlexibleContexts #-}

import Control.Applicative (liftA2)
import GI.Gtk hiding (main)
import Reflex.Spider
import Reflex.GI.Gtk
import Reflex.GI.Gtk.Run
import System.Environment
import System.Exit
import qualified System.HIDAPI as Hid
import Control.Monad
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU      -- from utf8-string
import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Text.Encoding as TLE
import qualified Reflex.Process as P

main :: IO ()
main = do
  d <- openDevice
  argv <- (:) <$> getProgName <*> getArgs
  Just gtkApplication <- applicationNew (Just "functor.love.JarneGUI") []
  runReflexGtk gtkApplication (Just argv) (myReactiveCode d gtkApplication) >>= \case
    0 -> putStrLn  "exit sucess" >> exitSuccess
    n -> do
      putStrLn ("exit failed: " <> show n)
      exitWith $ ExitFailure $ fromIntegral n

myReactiveCode :: MonadReflexGtk t m => Hid.Device -> Application -> m ()
myReactiveCode d gtkApplication = do
  window <- runGtk $ applicationWindowNew gtkApplication
  windowResize window 100 400
  box <- runGtk $ boxNew OrientationVertical 0
  containerAdd window box
  input1 <- runGtk entryNew
  input2 <- runGtk entryNew
  output <- runGtk $ labelNew Nothing

  runGtk $ boxPackStart box input1 False False 0
  runGtk $ boxPackStart box input2 False False 0
  runGtk $ boxPackStart box output False False 0


  text1 <- dynamicOnSignal "" input1 #changed $ \fire -> do
    s <- entryGetText input1
    -- runGtk_ (Hid.write d $ "lookup " <> TLE.encodeUtf8 s)
    fire s

  -- text2 <- dynamicOnAttribute o #text

  -- let combinedText = text1
  -- let combinedText = liftA2 (<>) text1 text2

  sink output [#label :== text1]

  _ <- gtkApplication `on` #activate $ widgetShowAll window

  o <- runGtk $ BSU.toString <$> Hid.read d 65
  pure ()

-- lookupJarne :: HasSpiderTimeline t => MonadReflexGtk t m => Hid.Device -> T.Text -> m ()
-- lookupJarne d s = do
--   runGtk_ (liftIO . Hid.write d $ "lookup " <> TLE.encodeUtf8 s)

openDevice :: IO Hid.Device
openDevice = do
  Hid.init
  -- -- ds <- Hid.enumerateAll
  ds <- Prelude.filter (\d -> Hid.usagePage d == jarneUsagePage) <$> Hid.enumerate (Just jarneProductId) (Just jarneVendorId)

  forM_ ds $ \d -> print d -- (productString d, productId d, vendorId d)
  di <- if Prelude.length ds == 1 then pure (Prelude.head ds) else do
            putStrLn "which device would you like to use?"
            nstr <- Prelude.getLine
            pure $ ds !! (Prelude.read nstr :: Int)
  Hid.openDeviceInfo di

 where
  jarneVendorId = 1
  jarneProductId = 19530
  jarneSerialNumber = "fe:4c:2c:87:0d:db"
  jarneUsagePage = 65329 -- 0xff31 is the right one to use!


 --  repl d
 --  --   Hid.write d "lookup test" >>= print
 --  close d

 --  -- forM_ ds $ \di -> do
 --  --   print di
 --  --   d <- Hid.openDeviceInfo di
 --  --   Hid.write d "lookup test" >>= print
 --  --   close d
 --  Hid.exit
 -- where
 --  -- loop :: MVar () -> IO () -> IO ()
 --  -- loop v act = act >> tryTakeMVar v >>= \case
 --  --   Just _ -> pure ()
 --  --   Nothing -> void $ loop v act
 --  repl :: Device -> IO ()
 --  repl d = do
 --    BS.putStr "lookup: "
 --    hFlush stdout
 --    BS.getLine >>= \case
 --      "bye" -> return ()
 --      word    -> do
 --        void $ Hid.write d ("lookup " <> word <> "\n")
 --        repl d
