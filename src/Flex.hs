{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Flex where

import Data.Function ((&))
import Dimension
import Geometry
import Style

maybeClamp :: Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float
maybeClamp (Just val) minValCell maxValCell =
  let clamped = case minValCell of
        Just minVal -> max val minVal
        Nothing -> val
   in Just $ case maxValCell of
        Just maxVal -> min clamped maxVal
        Nothing -> clamped
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

-- | Calculate the size of a block item.
layoutSize ::
  Style ->
  Size (Maybe Float) ->
  Size (Maybe Float) ->
  Size AvailableSpace ->
  Size (Maybe Float)
layoutSize style knownDims parentSize availableSpace =
  let 
      minSize = fMaybeToAbs style.minSize parentSize
      maxSize = fMaybeToAbs style.maxSize parentSize
      size = fMaybeToAbs style.size parentSize
      clampedSize = maybeClamp <$> size <*> minSize <*> maxSize
      availableSpaceSize = Size (intoPixels availableSpace.width) Nothing
   in maybeOr <$> knownDims <*> (maybeOr <$> clampedSize <*> availableSpaceSize)

data AvailableSpace = Pixels Float | MinContent | MaxContent

intoPixels :: AvailableSpace -> Maybe Float
intoPixels (Pixels px) = Just px
intoPixels _ = Nothing

mkLayout :: Style -> Size AvailableSpace -> Size (Maybe Float)
mkLayout style availableSpace =
  layoutSize
    style
    (pure Nothing)
    (fmap intoPixels availableSpace)
    availableSpace
