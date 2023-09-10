{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Lib
  ( Node (..),
    Style (..),
    Size (..),
    FixedDimension (..),
    Dimension(..),
    defaultStyle,
    Direction (..),
    layoutSize,
    fMaybeToAbs
  )
where

import Data.Function ((&))

data FixedDimension = Points Float | Percent Float deriving (Show)

toAbs :: FixedDimension -> Float -> Float
toAbs (Points points) _ = points
toAbs (Percent percent) parent = percent * parent

maybeToAbs :: Maybe FixedDimension -> Maybe Float -> Maybe Float
maybeToAbs (Just (Points points)) _ = Just points
maybeToAbs (Just (Percent percent)) (Just parent) = Just $ percent * parent
maybeToAbs _ _ = Nothing

data Dimension = Fixed FixedDimension | Auto deriving (Show)

toFixed :: Dimension -> Maybe FixedDimension
toFixed (Fixed dim) = Just dim
toFixed Auto = Nothing

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

sizeToAbs :: (Applicative f) => f FixedDimension -> f Float -> f Float
sizeToAbs dimensionSize parentSize = toAbs <$> dimensionSize <*> parentSize

fMaybeFixedToAbs :: (Applicative f) => f (Maybe FixedDimension) -> f (Maybe Float) -> f (Maybe Float)
fMaybeFixedToAbs dimensionSize parentSize = maybeToAbs <$> dimensionSize <*> parentSize

fMaybeToAbs :: (Applicative f) => f Dimension -> f (Maybe Float) -> f (Maybe Float)
fMaybeToAbs size = fMaybeFixedToAbs (fmap toFixed size)

maybeClamp :: Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float
maybeClamp (Just val) minVal maxVal =
  let val = case minVal of
        Just minVal -> max val minVal
        Nothing -> val
   in Just $ case maxVal of
        Just maxVal -> min val maxVal
        Nothing -> val
maybeClamp Nothing _ _ = Nothing

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

data BlockItem = BlockItem
  { order :: Int,
    size :: Size (Maybe Float)
  }

mkItems styles innerSize =
  filter (\style -> style.display /= None) styles
    & zipWith
      ( \order style ->
          BlockItem
            { order = order,
              size = fMaybeFixedToAbs style.size innerSize
            }
      )
      [0 ..]

maybeOr (Just a) _ = Just a
maybeOr Nothing b = b

layoutSize :: Style -> Size (Maybe Float) -> Size (Maybe Float) -> Size (Maybe Float)
layoutSize style knownDims parentSize =
  let minSize = fMaybeToAbs style.minSize parentSize
      maxSize = fMaybeToAbs style.maxSize parentSize
      size = fMaybeToAbs style.size parentSize
      clampedSize = maybeClamp <$> size <*> minSize <*> maxSize
   in maybeOr <$> knownDims <*> size
