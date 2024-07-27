-- example code from termbox-banana docs
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Data.Maybe
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Control.Lens as L
import Control.Lens.TH
import Data.Foldable (fold)
import Data.Function ((&))
import Reactive.Banana ((<@>))
import Reactive.Banana qualified as Banana
import Termbox.Banana qualified as Termbox
import GHC.Generics (Generic)


data Q final builder = Q
  { _history :: Vector final
  , _current :: builder
  } deriving (Show, Eq)
$(makeLenses ''Q)
type Query = Q String (Vector Char)

instance (Semigroup a, Semigroup b) => Semigroup (Q a b) where
  l <> r = Q (l ^. history <> r ^.history) (l ^. current <> r ^.current)
instance (Monoid a, Monoid b) => Monoid (Q a b) where
  mempty = Q mempty mempty
  
renderCurrent :: Query -> String
renderCurrent = Vector.toList . (L.view current) 

lastQuery :: Query -> String
lastQuery = Vector.head . (L.view history) 


build :: Monoid a => Monoid b => Q a b
build = mempty

main :: IO ()
main = do
  result <- Termbox.run network
  putStrLn case result of
    Left err -> "Termbox program failed to initialize: " ++ show err
    Right state -> "Final state: " ++ show state

-- TODO: just make this a constrained fingertree?
data In
  = In Char
  | Bksp
  | Done
  -- TODO
  -- | Left | Right | Home | End | WordLeft | WordRight | Del | WordLeftDel | WordRightDel | WordLeftBksp | WordRightBksp
  | Noop
  deriving (Show, Eq)

key2input :: Termbox.Key -> In
key2input = \case
  Termbox.KeyChar c -> In c
  Termbox.KeySpace -> In ' '
  Termbox.KeyBackspace -> Bksp
  Termbox.KeyEnter -> Done
  _ -> Noop

-- renderIns :: [In] -> Query
-- renderIns = go mempty where
--   go :: QueryPart -> [In] -> Query
--   go fin [] = Q fin
--   go m (In c:rst) = go (m `Vector.snoc` c) rst
--   go m (Bksp:rst) = go (Vector.init m) rst
--   go m (Done:rst) = Finished (Vector.toList m)
--   go m (Noop:rst) = go m rst

network :: (Banana.MonadMoment m) => Termbox.Inputs -> m (Termbox.Outputs Query)
network inputs = do
  q <- Banana.accumB build (act <$> inputs.keys)
  pure
    Termbox.Outputs
      { scene = render <$> q
      -- { scene = undefined
      , done = Banana.filterJust (isDone <$> q <@> inputs.keys)
      -- , done = undefined
      }
  where
    isDone :: Query -> Termbox.Key -> Maybe Query
    isDone n = \case
      Termbox.KeyEsc -> Just n
      _ -> Nothing

    act :: Termbox.Key -> Query -> Query
    act k q = case key2input k of
         In c -> q & current %~ (`Vector.snoc` c)
         Bksp -> q & current %~ Vector.init
         Done -> Q { _current = mempty, _history = (Vector.toList (q ^. current)) `Vector.cons` (q ^. history) }
         Noop -> q

render :: Query -> Termbox.Scene
render q =
  fold
    [ string $ "current: " ++ renderCurrent q
    --, string ( "previous: " ++ Vector.head (q ^. history) )
    --    & Termbox.atRow 2
    , fold
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
