module GenGol.Grid (Grid (..), initGrid, nextState) where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import GenGol.Types
import System.Random

newtype Grid a = Grid (Dimension, Vector (Vector a))
  deriving (Show)

initGrid :: Int -> Dimension -> Grid Cell
initGrid seed dims@(m, n) = Grid (dims, rows)
  where
    rows :: Vector (Vector Cell)
    rows = V.generate m cells

    cells :: Int -> Vector Cell
    cells r = V.generate n (genCell r)

    genCell :: Int -> Int -> Cell
    genCell r c
      | rands ! (r * n + c) = Alive
      | otherwise = Dead

    rands :: Vector Bool
    rands = V.fromListN (m * n) $ randoms gen

    gen :: StdGen
    gen = mkStdGen seed

nextState :: Grid Cell -> Grid Cell
nextState = fromNbrs isAlive . toNbrs

fromNbrs :: ((Cell, [Cell]) -> Cell) -> Grid (Cell, [Cell]) -> Grid Cell
fromNbrs f (Grid (dims, nbrs)) = Grid (dims, grid)
  where
    grid :: Vector (Vector Cell)
    grid = V.map (V.map f) nbrs

isAlive :: (Cell, [Cell]) -> Cell
isAlive (cell, nbrs) = case cell of
  Alive
    | 1 < numAlive && numAlive <= 3 -> Alive
    | otherwise -> Dead
  Dead
    | numAlive == 3 -> Alive
    | otherwise -> Dead
  where
    numAlive = length (filter (== Alive) nbrs)

toNbrs :: Grid Cell -> Grid (Cell, [Cell])
toNbrs (Grid (dims, grid)) = Grid (dims, nbrGrid)
  where
    nbrGrid :: Vector (Vector (Cell, [Cell]))
    nbrGrid = V.imap (V.imap . curry cellNbrs) grid

    cellNbrs :: Point -> Cell -> (Cell, [Cell])
    cellNbrs point cell = (cell, neighbours point)

    neighbours :: Point -> [Cell]
    neighbours point = cell <$> adjacent dims point

    cell :: Point -> Cell
    cell (r, c) = grid ! r ! c

adjacent :: Dimension -> Point -> [Point]
adjacent (m, n) (r, c) =
  [ (row, col)
    | i <- dirs,
      j <- dirs,
      (i, j) /= (0, 0),
      let row = (i + r) `mod` m,
      let col = (j + c) `mod` n
  ]
  where
    dirs = [-1 .. 1]
