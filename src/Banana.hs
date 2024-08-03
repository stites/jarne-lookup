{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
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
import Termbox.Banana qualified as Termbox
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.ByteString.UTF8 qualified as BSU      -- from utf8-string
import Data.ByteString.Lazy.UTF8 qualified as BLU -- from utf8-string
import System.HIDAPI qualified as Hid


import Data.Text.IO qualified as TIO
data Q builder final = Q
  { _current :: builder
  -- , _history :: Vector final
  -- , _key :: Vector Termbox.Key
  -- , _response :: [Entry]
  } deriving (Show, Eq)
$(L.makeLenses ''Q)
type Query = Q (Vector Char) String
-------------------------------------------------------



main :: IO ()
main = do
    Hid.init
    Just d <- Hid.firstDevice
    ref <- newIORef T.empty
    sources <- makeSources
    network <- compile $ networkDescription sources
    activate network
    eventLoop ref d sources

-- Create event sources corresponding to  query  and  resp
makeSources :: IO (EventSource Text, EventSource (Maybe [Entry]))
makeSources = (,) <$> newAddHandler <*> newAddHandler

-- Read commands and fire corresponding events
eventLoop :: IORef Text -> Hid.Device -> (EventSource Text, EventSource (Maybe [Entry])) -> IO ()
eventLoop r d (equery, eresult) = loop where
  loop = do
    putStr "> "
    hFlush stdout
    s <- getLine
    fire equery (T.pack s)
    case s of
      "q" -> pure ()
      "exit" -> pure ()
      "quit" -> pure ()
      ('l':'o':'o':'k':'u':'p':' ': query) -> do
        lookupdev d (T.pack query)
        readdev r d >>= fire eresult
        loop
      s -> do
        putStrLn $ "unknown query: " <> s
        loop

  lookupdev :: Hid.Device -> Text -> IO ()
  lookupdev d word = void $ Hid.write d (T.encodeUtf8 $ "lookup " <> word <> "\n")

  reset :: IORef Text -> IO ()
  reset = flip writeIORef mempty
  
  readdev :: IORef Text -> Hid.Device -> IO (Maybe [Entry])
  readdev r d = do
    out <- stripEnd . T.pack . BSU.toString <$> Hid.read d 65
    if T.null out then reset r >> pure Nothing else do
      f <- (T.strip . (<> out)) <$> readIORef r
      if err `T.isSuffixOf` f
      then 
        case gettit . T.strip . fromMaybe "" . T.stripSuffix err $ f of
          Left  e  -> reset r >> pure Nothing
          Right es -> reset r >> pure (Just es)
      else
        case gettit f of
          Right es ->        reset r >> pure (Just es)
          Left  e  -> writeIORef r f >> readdev r d

  err :: Text
  err = "ERR Invalid command. Use \"help\" for a list of commands"
            
  gettit :: Text -> Either String [Entry]
  gettit t = (eitherDecode (BLU.fromString (T.unpack $ t)) )

  stripEnd = T.dropWhileEnd (\c -> c == '\NUL' || c == ' ' || c == '\n')

{-----------------------------------------------------------------------------
    Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

type StdGen = (Int, Int)
newStdGen = pure (0, 0)
randomR (l, r) = pure (3, (0, 0))
{-----------------------------------------------------------------------------
    Program logic
------------------------------------------------------------------------------}
type Money = Int
-- State of the reels, consisting of three numbers from 1-4. Example: "222"
type Reels = (Int,Int,Int)
-- A win consist of either double or triple numbers
data Win = Double | Triple


-- Program logic in terms of events and behaviors.
networkDescription :: (EventSource Text, EventSource (Maybe [Entry])) -> MomentIO ()
networkDescription (esquery, esresp) = mdo
    -- initial random number generator
    initialStdGen <- liftIO $ newStdGen

    -- Obtain events corresponding to the  query  and  resp  commands
    equery :: Event Text
      <- fromAddHandler (addHandler esquery)
    eresp  :: Event (Maybe [Entry])
      <- fromAddHandler (addHandler esresp)
    
    -- The state of the slot machine is captured in Behaviors.

    -- -- State: credits that the resper has to resp the game
    -- -- The  equery      event adds a query to the credits
    -- -- The  edoesresp  event removes money
    -- -- The  ewin       event adds credits because the resper has won
    -- (ecredits :: Event Money, bcredits :: Behavior Money)
    --     <- mapAccum 0 . fmap (\f x -> (f x,f x)) $ unions $
    --         [ addCredit    <$ equery
    --         , removeCredit <$ edoesresp
    --         , addWin       <$> ewin
    --         ]
    -- let
    --     -- functions that change the accumulated state
    --     addCredit     = (+1)
    --     removeCredit  = subtract 1
    --     addWin Double = (+5)
    --     addWin Triple = (+20)

    --     -- Event: does the resper have enough money to resp the game?
    --     emayresp :: Event Bool
    --     emayresp = (\credits _ -> credits > 0) <$> bcredits <@> eresp

    --     -- Event: resper has enough querys and resps
    --     edoesresp :: Event ()
    --     edoesresp = () <$ filterE id  emayresp
    --     -- Event: event that fires when the resper doesn't have enough money
    --     edenied   :: Event ()
    --     edenied   = () <$ filterE not emayresp


    -- -- State: random number generator
    -- (eroll :: Event Reels, bstdgen :: Behavior StdGen)
    --     -- accumulate the random number generator while rolling the reels
    --     <- mapAccum initialStdGen $ roll <$> edoesresp

    -- let
    --     -- roll the reels
    --     roll :: () -> StdGen -> (Reels, StdGen)
    --     roll () gen0 = ((z1,z2,z3),gen3)
    --         where
    --         random    = randomR(1,4)
    --         (z1,gen1) = random gen0
    --         (z2,gen2) = random gen1
    --         (z3,gen3) = random gen2

    --     -- Event: it's a win!
    --     ewin :: Event Win
    --     ewin = fmap fromJust $ filterE isJust $ fmap checkWin eroll
    --     checkWin (z1,z2,z3)
    --         | length (nub [z1,z2,z3]) == 1 = Just Triple
    --         | length (nub [z1,z2,z3]) == 2 = Just Double
    --         | otherwise                    = Nothing


    -- -- ecredits <- changes bcredits
    -- reactimate $ putStrLn . show <$> equery
    reactimate $ putStrLn . show <$> eresp
    -- reactimate $ putStrLn . showWin    <$> ewin
    -- reactimate $ putStrLn "Not enough credits!" <$ edenied
    pure ()


showCredit money    = "Credits: " ++ show money
showRoll (z1,z2,z3) = "You rolled  " ++ show z1 ++ show z2 ++ show z3
showWin Double = "Wow, a double!"
showWin Triple = "Wowwowow! A triple! So awesome!"

-- {-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

-- module Main (main) where

-- import Control.Arrow ((&&&))
-- import Lib qualified as Hid
-- import Lib (Entry)
-- import Data.Maybe (fromMaybe)
-- import Data.Vector (Vector)
-- import Data.Vector qualified as Vector
-- import Control.Lens ((&), (^.), (%~))
-- import Control.Lens qualified as L
-- import Data.Foldable (fold)
-- import Reactive.Banana ((<@>), Event, Behavior)
-- import Reactive.Banana qualified as Banana
-- import Reactive.Banana.Frameworks (MomentIO)
-- import Reactive.Banana.Frameworks qualified as Banana
-- import Control.Event.Handler qualified as H
-- import Termbox.Banana qualified as Termbox

-- data Q final builder = Q
--   { _history :: Vector final
--   , _current :: builder
--   , _key :: Vector Termbox.Key
--   , _response :: [Entry]
--   } deriving (Show, Eq)
-- $(L.makeLenses ''Q)
-- type Query = Q String (Vector Char)

-- instance (Semigroup a, Semigroup b) => Semigroup (Q a b) where
--   l <> r = Q (l ^. history <> r ^.history) (l ^. current <> r ^.current) (l ^. key <> r ^.key) (l ^. response <> r ^.response)
-- instance (Monoid a, Monoid b) => Monoid (Q a b) where
--   mempty = Q mempty mempty mempty mempty

-- renderCurrent :: Query -> String
-- renderCurrent = Vector.toList . L.view current

-- safe :: (Vector a -> b) -> Vector a -> Maybe b
-- safe opr v =
--   if null v
--   then Nothing
--   else Just $ opr v

-- prevQuery :: Query -> String
-- prevQuery q = fromMaybe mempty $ safe Vector.head (q ^. history)

-- historyTail :: Query -> Vector String
-- historyTail q = fromMaybe mempty $ safe Vector.tail (q ^. history)


-- type Database a = (H.AddHandler a, a -> IO ())
-- type Db = Database String

-- addHandler :: EventSource a -> AddHandler a
-- addHandler = fst

-- setupNetwork :: Database EventNetwork -> IO EventNetwork
-- setupNetwork espause = compile $ do
--     counterUp   <- fromAddHandler (addHandler eplus)
--     counterDown <- fromAddHandler (addHandler eminus)
--     epause      <- fromAddHandler (addHandler espause)

--     ecount <- accumE 0 $ unions
--         [ (+1)       <$ counterUp
--         , subtract 1 <$ counterDown
--         ]

--     reactimate $ fmap print ecount
--     reactimate $ fmap pause epause




-- main :: IO ()
-- main = do
--   Hid.init
--   Just d <- Hid.firstDevice
--   ps <- putter
--   result <- Termbox.run (network d ps)
--   putStrLn case result of
--     Left err -> "Termbox program failed to initialize: " ++ show err
--     Right state -> "Final state: " ++ show state

-- -- TODO: just make this a constrained fingertree?
-- data In
--   = In Char
--   | Bksp
--   | Done
--   | Noop
--   deriving (Show, Eq)

-- key2input :: Termbox.Key -> In
-- key2input = \case
--   Termbox.KeyChar c -> In c
--   Termbox.KeySpace -> In ' '
--   Termbox.KeyBackspace -> Bksp
--   Termbox.KeyDelete -> Bksp
--   Termbox.KeyEnter -> Done
--   _ -> Noop

-- network :: Hid.Device -> Putter -> Termbox.Inputs -> MomentIO (Termbox.Outputs Query)
-- network d (rput, put) inputs = do
--   ki <- Banana.accumE (Termbox.KeyCtrl2, Noop) ((\k _ -> (id &&& key2input) k) <$> inputs.keys)
--   bx <- Banana.fromChanges [] rput
--   -- ex <- Banana.fromChanges [] rput
--   -- eput <- Banana.fromAddHandler rput
--   -- qb <- case i of
--   --   Noop -> Banana.accumB mempty $ (\k q -> q & key %~ (`Vector.snoc` k)) <$> inputs.keys
--   --   In c -> Banana.accumB mempty $ (\k q -> q & current %~ (`Vector.snoc` c) & key %~ (`Vector.snoc` k)) <$> inputs.keys
--   --   Bksp -> Banana.accumB mempty $ (\k q -> q & current %~ Vector.init       & key %~ (`Vector.snoc` k)) <$> inputs.keys
--   --   -- Done -> Q
--   --   --        { _current = mempty
--   --   --        , _history = consHist q
--   --   --        , _key = mempty
--   --   --        , _response = []
--   --   --        }


--   -- -- qe <- Banana.accumE mempty (act <$> ki)
--   -- -- qb <- Banana.accumB mempty ((\q _ -> q) <$> qe)
--   qb <- Banana.accumB mempty $ act <$> ki



--   pure
--     Termbox.Outputs
--       { scene = render <$> qb
--       -- { scene = undefined
--       , done = Banana.filterJust (isDone <$> qb <@> inputs.keys)
--       -- , done = undefined
--       }
--   where
--     isDone :: Query -> Termbox.Key -> Maybe Query
--     isDone n = \case
--       Termbox.KeyEsc -> Just n
--       _ -> Nothing

--     consHist :: Query -> Vector String
--     consHist q = Vector.cons (renderCurrent q) (q ^. history)

--     act :: (Termbox.Key, In) -> Query -> Query
--     act (k, i) q =
--       case i of
--          In c -> (q & current %~ (`Vector.snoc` c)) & key %~ (`Vector.snoc` k)
--          Bksp -> (q & current %~ Vector.init      ) & key %~ (`Vector.snoc` k)
--          -- Bksp -> build
--          Done -> Q
--            { _current = mempty
--            , _history = consHist q
--            , _key = mempty
--            , _response = []
--            }
--          Noop -> q & key %~ (`Vector.snoc` k)

-- {-# ANN module "HLint: ignore Redundant flip" #-}
-- render :: Query -> Termbox.Scene
-- render q =
--   fold
--     ([ fold
--         [ string "Press",
--           string "Esc" & Termbox.bold & Termbox.atCol 6,
--           string "to quit." & Termbox.atCol 10
--         ]
--     , string ("current: " ++ renderCurrent q)
--         & Termbox.atRow 2
--     , string (maybe "" show $ safe Vector.last $ q ^. key)
--         & Termbox.atRow 2
--         & Termbox.atCol 50
--     , string ("history: " ++ prevQuery q)
--         & Termbox.atRow 3
--     ] <> Vector.toList (flip Vector.imap (historyTail q) (\i query ->
--       string ("       - " <> query)
--         & Termbox.atRow (4 + i))))
--     & Termbox.at Termbox.Pos {row = 2, col = 4}
--     & Termbox.image

-- string :: [Char] -> Termbox.Image
-- string chars =
--   zip [0 ..] chars & foldMap \(i, char) ->
--     Termbox.char char & Termbox.atCol i
