module Style where
import Geometry
import Dimension

data Direction = Row | Column deriving (Show)

data Display = Block | None deriving (Eq, Show)

data Style = Style
  { size :: Size Dimension,
    minSize :: Size Dimension,
    maxSize :: Size Dimension,
    direction :: Direction,
    display :: Display
  }
  deriving (Show)

defaultStyle :: Style
defaultStyle =
  Style
    { display = Block,
      size = pure . Fixed $ Points 0,
      minSize = pure . Fixed $ Points 0,
      maxSize = pure Auto,
      direction = Row
    }
