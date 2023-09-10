{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Flex where

import Data.Function ((&))
import Dimension
import Geometry
import Style

maybeClamp :: Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float
maybeClamp (Just val) minVal maxVal =
  let val = case minVal of
        Just minVal -> max val minVal
        Nothing -> val
   in Just $ case maxVal of
        Just maxVal -> min val maxVal
        Nothing -> val
maybeClamp Nothing _ _ = Nothing

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

mkItems :: [Style] -> Size (Maybe Float) -> [BlockItem]
mkItems styles innerSize =
  filter (\style -> style.display /= None) styles
    & zipWith
      ( \order style ->
          BlockItem
            { order = order,
              size = fMaybeToAbs style.size innerSize
            }
      )
      [0 ..]

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just a) _ = Just a
maybeOr Nothing b = b

layoutSize :: Style -> Size (Maybe Float) -> Size (Maybe Float) -> Size (Maybe Float)
layoutSize style knownDims parentSize =
  let minSize = fMaybeToAbs style.minSize parentSize
      maxSize = fMaybeToAbs style.maxSize parentSize
      size = fMaybeToAbs style.size parentSize
      clampedSize = maybeClamp <$> size <*> minSize <*> maxSize
   in maybeOr <$> knownDims <*> size
