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
import Data.Foldable (fold)
import Reactive.Banana ((<@>))
import Reactive.Banana qualified as Banana
import Termbox.Banana qualified as Termbox

data Q final builder = Q
  { _history :: Vector final
  , _current :: builder
  , _key :: Vector Termbox.Key
  } deriving (Show, Eq)
$(makeLenses ''Q)
type Query = Q String (Vector Char)

instance (Semigroup a, Semigroup b) => Semigroup (Q a b) where
  l <> r = Q (l ^. history <> r ^.history) (l ^. current <> r ^.current) (l ^. key <> r ^.key)
instance (Monoid a, Monoid b) => Monoid (Q a b) where
  mempty = Q mempty mempty mempty
  
renderCurrent :: Query -> String
renderCurrent = Vector.toList . L.view current

safe :: (Vector a -> b) -> Vector a -> Maybe b
safe opr v =
  if null v
  then Nothing
  else Just $ opr v

prevQuery :: Query -> String
prevQuery q = fromMaybe mempty $ safe Vector.head (q ^. history)

historyTail :: Query -> Vector String
historyTail q = fromMaybe mempty $ safe Vector.tail (q ^. history)

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
  Termbox.KeyDelete -> Bksp
  Termbox.KeyEnter -> Done
  _ -> Noop

network :: (Banana.MonadMoment m) => Termbox.Inputs -> m (Termbox.Outputs Query)
network inputs = do
  q <- Banana.accumB mempty (act <$> inputs.keys)
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
    act k q =
      case key2input k of
         In c -> (q & current %~ (`Vector.snoc` c)) & key %~ (`Vector.snoc` k)
         Bksp -> (q & current %~ Vector.init      ) & key %~ (`Vector.snoc` k)
         -- Bksp -> build
         Done -> Q { _current = mempty, _history = Vector.toList (q ^. current) `Vector.cons` (q ^. history), _key = mempty }
         Noop -> q & key %~ (`Vector.snoc` k)

{-# ANN module "HLint: ignore Redundant flip" #-}
render :: Query -> Termbox.Scene
render q =
  fold
    ([ fold
        [ string "Press",
          string "Esc" & Termbox.bold & Termbox.atCol 6,
          string "to quit." & Termbox.atCol 10
        ]
    , string ("current: " ++ renderCurrent q)
        & Termbox.atRow 2
    , string (maybe "" show $ safe Vector.last $ q ^. key)
        & Termbox.atRow 2
        & Termbox.atCol 50
    , string ("history: " ++ prevQuery q)
        & Termbox.atRow 3
    ] <> Vector.toList (flip Vector.imap (historyTail q) (\i query ->
      string ("       - " <> query)
        & Termbox.atRow (4 + i))))
    & Termbox.at Termbox.Pos {row = 2, col = 4}
    & Termbox.image

string :: [Char] -> Termbox.Image
string chars =
  zip [0 ..] chars & foldMap \(i, char) ->
    Termbox.char char & Termbox.atCol i
 
