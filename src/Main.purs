module Main where

import Prelude
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.List (List(..))
import Data.List.Zipper (DoubleZipper(..), Zipper(..))
import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Engine (class Drawable, GameLoop, Window(..), defaultConfig, draw, flush, fromArray, runGameLoop, translate)
import Engine.Ticker (tick)

data Tetromino
  = I
  | O
  | T
  | L
  | J
  | S
  | Z

data Block
  = BMagenta
  | BYellow
  | BPurple
  | BOrange
  | BBlue
  | BGreen
  | Red

type Field
  = DoubleZipper (Maybe Block)

rows :: Int
rows = 20

cols :: Int
cols = 10

initField :: Field
initField = DoubleZipper $ Zipper Nil line (replicate (rows - 1) line)
  where
  line :: Zipper (Maybe Block)
  line = Zipper Nil Nothing (replicate (cols - 1) Nothing)

data Game
  = Game Tetromino Field

tetrominoI :: forall m. Drawable m => m Unit
tetrominoI = translate { x: -2, y: 0, z: 0 } $ fromArray [ "####" ]

testLoop :: GameLoop Unit
testLoop = do
  forever
    $ do
        lift $ draw tetrominoI
        lift $ flush
        tick

main :: Effect Unit
main = do
  runGameLoop (defaultConfig { window = FullScreen }) testLoop
