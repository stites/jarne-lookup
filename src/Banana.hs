{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}


    -- allows recursive do notation
    -- mdo
    --     ...

import Control.Monad (when, void)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
-- import System.Random
import System.IO
import Debug.Trace
import Data.IORef

import Reactive.Banana as R
import Reactive.Banana.Frameworks as R

-----------------------------
import Control.Arrow ((&&&))
import Lib qualified as Hid
import Lib (Entry)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Control.Lens ((&), (^.), (%~))
import Control.Lens qualified as L
import Data.Foldable (fold)
import Reactive.Banana ((<@>), Event, Behavior)
import Data.Aeson
import Reactive.Banana qualified as Banana
import Reactive.Banana.Frameworks (MomentIO)
import Reactive.Banana.Frameworks qualified as Banana
import Control.Event.Handler qualified as H
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS      -- from utf8-string
import Data.ByteString.UTF8 qualified as BSU      -- from utf8-string
import Control.Concurrent.MVar
import Termbox.Banana (Inputs(..), Outputs(..))
import Termbox.Banana qualified as Termbox
import Data.Void
import Termbox (poll)
import qualified Termbox as TERMBOX -- (Event (..), render, run)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.ByteString.UTF8 qualified as BSU      -- from utf8-string
import Data.ByteString.Lazy.UTF8 qualified as BLU -- from utf8-string
import System.HIDAPI qualified as Hid


import Data.Text.IO qualified as TIO
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import System.Random
import System.IO
import System.Environment
import Debug.Trace
import Data.IORef

import Reactive.Banana
import Reactive.Banana.Frameworks



import Data.Foldable (fold)
import Data.Function ((&))
import Reactive.Banana ((<@>))
import Reactive.Banana qualified as Banana
import Termbox.Banana qualified as Termbox

main :: IO ()
main = do
  result <- Termbox.run network
  putStrLn case result of
    Left err -> "Termbox program failed to initialize: " ++ show err
    Right state -> "Final state: " ++ show state

network :: Banana.MonadMoment m => Termbox.Inputs -> m (Termbox.Outputs Int)
network inputs = do
  keysPressed <- Banana.accumB 0 ((+ 1) <$ inputs.keys)
  pure $ Termbox.Outputs
    { scene = render <$> keysPressed,
      done = Banana.filterJust (isDone <$> keysPressed <@> inputs.keys)
    }
  where
    isDone :: Int -> Termbox.Key -> Maybe Int
    isDone n = \case
      Termbox.KeyEsc -> Just n
      _ -> Nothing

render :: Int -> Termbox.Scene
render keysPressed = fold
  [ string ("Number of keys pressed: " ++ show keysPressed),
    fold
      [ string "Press",
        string "Esc" & Termbox.bold & Termbox.atCol 6,
        string "to quit." & Termbox.atCol 10
      ]
      & Termbox.atRow 2
  ]
  & Termbox.at Termbox.Pos {row = 2, col = 4}
  & Termbox.image

string :: [Char] -> Termbox.Image
string chars =
  zip [0 ..] chars & foldMap \(i, char) ->
    Termbox.char char & Termbox.atCol i
