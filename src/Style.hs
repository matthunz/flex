module Style where

import Dimension
import Geometry

data Direction = Row | Column deriving (Show)

data Display = Block | None deriving (Eq, Show)

data Style = Style
  { size :: Size Dimension,
    minSize :: Size Dimension,
    maxSize :: Size Dimension,
    direction :: Direction,
    display :: Display,
    margin :: Rect Dimension,
    border :: Rect FixedDimension,
    padding :: Rect FixedDimension
  }
  deriving (Show)

defaultStyle :: Style
defaultStyle =
  Style
    { display = Block,
      size = pure . Fixed $ Points 0,
      minSize = pure . Fixed $ Points 0,
      maxSize = pure Auto,
      direction = Row,
      margin = pure . Fixed $ Points 0,
      border = pure $ Points 0,
      padding = pure $ Points 0
    }
