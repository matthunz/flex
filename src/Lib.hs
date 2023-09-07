{-# LANGUAGE DataKinds #-}

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
  fmap f size = Size {width = f $ width size, height = f $ height size}

instance Applicative Size where
  pure a = Size {width = a, height = a}

  Size {width = f, height = g} <*> size =
    Size {width = f $ width size, height = g $ width size}

sizeToAbs dimensionSize parentSize = toAbs <$> dimensionSize <*> parentSize

data Direction = Row | Column deriving (Show)

data Style = Style
  { size :: Size Dimension,
    direction :: Direction
  }
  deriving (Show)

defaultStyle = Style {
  size = Size { width = Points 0 , height = Points 0},
  direction = Row
}

data Node = Node
  { style :: Style,
    nodes :: [Node]
  }
  deriving (Show)

data Layout = Layout
  { point :: Point Float,
    layoutSize :: Size Float
  }
  deriving (Show)

data LayoutNode = LayoutNode Layout [LayoutNode] deriving (Show)

layout :: Node -> Size Float -> LayoutNode
layout node outerSize = layout' node outerSize (Point 0 0)
  where
    layout' childNode parentSize pos =
      LayoutNode
        (Layout {point = pos, layoutSize = s})
        (snd $ foldr f (pos, []) (nodes childNode))
      where
        s = sizeToAbs (size (style childNode)) parentSize
        f n (p, acc) =
          ( case direction (style childNode) of
              Row -> Point {x = x p + width (layoutSize nodeLayout), y = y p}
              Column -> Point {x = x p, y = y p + height (layoutSize nodeLayout)},
            acc ++ [LayoutNode nodeLayout nodeChildren]
          )
          where
            (LayoutNode nodeLayout nodeChildren) = layout' n s p
