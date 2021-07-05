module GenGol.Types
  ( Dimension,
    Point,
    Cell (..),
  )
where

type Dimension = (Int, Int)

type Point = (Int, Int)

data Cell = Dead | Alive
  deriving stock (Eq, Show)
