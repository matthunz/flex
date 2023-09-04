{-# LANGUAGE DataKinds #-}

module Lib
  ( Node (..),
    Style (..),
    Size (..),
    Dimension (..),
    layout,
  )
where

data Dimension = Points Float | Percent Float deriving (Show)

toAbs :: Dimension -> Float -> Float
toAbs (Points points) _ = points
toAbs (Percent percent) parent = percent * parent

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

data Style = Style
  { size :: Size Dimension
  }
  deriving (Show)

data Node = Node
  { style :: Style,
    nodes :: [Node]
  }
  deriving (Show)

data LayoutNode = Result (Size Float) [LayoutNode] deriving (Show)

layout :: Node -> Size Float -> LayoutNode
layout node parentSize = Result s (map (`layout` s) (nodes node))
  where
    s = sizeToAbs (size (style node)) parentSize
