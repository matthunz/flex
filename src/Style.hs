module Style
  ( -- * Style
    Style (..),
    defaultStyle,

    -- * Properties
    Direction (..),
    Display (..),
  )
where

import Dimension (Dimension (..), FixedDimension (Points))
import Geometry (Rect, Size)

-- | Direction to flow a node's children.
data Direction = Row | Column deriving (Show)

-- | Display kind of a node.
data Display = Block | None deriving (Eq, Show)

-- | Layout style of a node.
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

-- | Default style of a node.
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
