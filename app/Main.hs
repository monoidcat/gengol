module Main where

import Control.Monad.Reader
import Control.Monad.Trans.Reader (ReaderT)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import GenGol
import GenGol.Types
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Options.Generic

data Env = Env
  { seed :: Int,
    gridSize :: Int,
    speed :: Int
  }
  deriving stock (Show, Generic)

instance ParseRecord Env

main :: IO ()
main = do
  env <- getRecord "Game of Life"
  runReaderT gameOfLife env

gameOfLife :: ReaderT Env IO ()
gameOfLife = do
  _seed <- asks seed
  _speed <- asks speed
  _gridSize <- asks gridSize

  let frameDims = (400, 400)
  let gridDims = (_gridSize, _gridSize)

  lift $
    simulate
      (window frameDims) -- run in windowed mode
      black -- background color
      _speed -- transitions per second
      (initGrid _seed gridDims) -- initial grid state
      (renderGrid frameDims) -- grid renderer
      step -- transition function

renderGrid :: Dimension -> Grid Cell -> Picture
renderGrid (w, h) (Grid ((m, n), grid)) =
  translate
    ((- width) / 2 + cellWidth / 2)
    (height / 2 - cellHeight / 2)
    $ pictures
      [ translate
          (fromIntegral c * cellHeight)
          (fromIntegral (- r) * cellWidth)
          (color (cellColor cell) (rectangleSolid cellWidth cellHeight))
        | r <- [0 .. m - 1],
          c <- [0 .. n - 1],
          let cell = grid ! r ! c
      ]
  where
    width :: Float
    width = fromIntegral w

    height :: Float
    height = fromIntegral h

    cellWidth :: Float
    cellWidth = width / fromIntegral m

    cellHeight :: Float
    cellHeight = height / fromIntegral n

    cellColor :: Cell -> Color
    cellColor Alive = white
    cellColor Dead = black

step :: ViewPort -> Float -> Grid Cell -> Grid Cell
step viewport time = nextState

window :: Dimension -> Display
window dims = InWindow "Game Of Life" dims (0, 0)
