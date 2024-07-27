-- example code from termbox-banana docs
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

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

network :: (Banana.MonadMoment m) => Termbox.Inputs -> m (Termbox.Outputs String)
network inputs = do
  lastKey <- Banana.accumB "<no key pressed>" (act <$> inputs.keys)
  pure
    Termbox.Outputs
      { scene = render <$> lastKey,
        done = Banana.filterJust (isDone <$> lastKey <@> inputs.keys)
      }
  where
    isDone :: String -> Termbox.Key -> Maybe String
    isDone n = \case
      Termbox.KeyEsc -> Just n
      _ -> Nothing
    act :: Termbox.Key -> String -> String
    act k prev = show k

render :: String -> Termbox.Scene
render key =
  fold
    [ string ("last key pressed: " ++ key),
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
