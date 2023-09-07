{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Lib
  ( Node (..),
    Style (..),
    Size (..),
    Dimension (..),
    defaultStyle,
    Direction (..),
    layout,
  )
where

data Dimension = Points Float | Percent Float deriving (Show)

toAbs :: Dimension -> Float -> Float
toAbs (Points points) _ = points
toAbs (Percent percent) parent = percent * parent

data Point t = Point
  { x :: t,
    y :: t
  }
  deriving (Show)

data Size t = Size
  { width :: t,
    height :: t
  }
  deriving (Show)

instance Functor Size where
  fmap f size = Size {width = f size.width, height = f size.height}

instance Applicative Size where
  pure a = Size {width = a, height = a}

  Size {width = f, height = g} <*> size =
    Size {width = f size.width, height = g size.height}

sizeToAbs :: (Applicative f) => f Dimension -> f Float -> f Float
sizeToAbs dimensionSize parentSize = toAbs <$> dimensionSize <*> parentSize

data Direction = Row | Column deriving (Show)

data Style = Style
  { size :: Size Dimension,
    direction :: Direction
  }
  deriving (Show)

defaultStyle :: Style
defaultStyle =
  Style
    { size = Size {width = Points 0, height = Points 0},
      direction = Row
    }

data Node = Node
  { style :: Style,
    nodes :: [Node]
  }
  deriving (Show)

data LayoutNode = LayoutNode
  { size :: Size Float,
    nodes :: [LayoutNode]
  }
  deriving (Show)

-- | Calculate the initial layout.
layout :: Node -> Size Float -> LayoutNode
layout node outerSize = layout' node outerSize (Point 0 0)
  where
    layout' childNode parentSize pos =
      LayoutNode
        size
        (snd $ foldr go (pos, []) childNode.nodes)
      where
        size = sizeToAbs childNode.style.size parentSize
        go innerNode (point, acc) =
          ( case direction childNode.style of
              Row -> point {x = point.x + width layoutNode.size}
              Column -> point {y = y point + height layoutNode.size},
            acc ++ [layoutNode]
          )
          where
            layoutNode = layout' innerNode size point
