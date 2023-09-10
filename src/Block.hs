{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Block where

import Data.Function ((&))
import Dimension
import Flex
import Geometry
import Style

mkLayout :: Node -> Size AvailableSpace -> Size Float
mkLayout node availableSpace =
  layout
    node
    (pure Nothing)
    (fmap intoPixels availableSpace)
    availableSpace

layout ::
  Node ->
  Size (Maybe Float) ->
  Size (Maybe Float) ->
  Size AvailableSpace ->
  Size Float
layout node knownDims parentSize availableSpace =
  let styleKnownDims = layoutSize node.style knownDims parentSize availableSpace
   in mkNodeLayout node styleKnownDims availableSpace

mkNodeLayout :: Node -> Size (Maybe Float) -> Size AvailableSpace -> Size Float
mkNodeLayout node knownDims availableSpace =
  let items = mkItems node.nodes knownDims
      containerOuterWidth = mkContentWidth items availableSpace.width
   in case knownDims.height of
        Just height -> Size {width = containerOuterWidth, height = height}
        Nothing -> error ""

data BlockItem = BlockItem
  { order :: Int,
    size :: Size (Maybe Float),
    node :: Node
  }

mkItems :: [Node] -> Size (Maybe Float) -> [BlockItem]
mkItems nodes innerSize =
  filter (\node -> node.style.display /= None) nodes
    & zipWith
      ( \order node ->
          BlockItem
            { order = order,
              size = fMaybeToAbs node.style.size innerSize,
              node = node
            }
      )
      [0 ..]

mkContentWidth :: [BlockItem] -> AvailableSpace -> Float
mkContentWidth items availableSpace =
  sum $
    map
      ( \item ->
          width $
            mkNodeLayout
              item.node
              (pure Nothing)
              Size {width = availableSpace, height = MinContent}
      )
      items

-- | Calculate the size of a block item.
layoutSize ::
  Style ->
  Size (Maybe Float) ->
  Size (Maybe Float) ->
  Size AvailableSpace ->
  Size (Maybe Float)
layoutSize style knownDims parentSize availableSpace =
  let minSize = fMaybeToAbs style.minSize parentSize
      maxSize = fMaybeToAbs style.maxSize parentSize
      size = fMaybeToAbs style.size parentSize
      clampedSize = maybeClamp <$> size <*> minSize <*> maxSize

      -- If both min and max in a given axis are set and max <= min
      -- then this determines the size in that axis.
      minMaxSize =
        ( \maybeMin maybeMax ->
            case (maybeMin, maybeMax) of
              (Just minVal, Just maxVal) ->
                if maxVal <= minVal then Just minVal else Nothing
              _ -> Nothing
        )
          <$> minSize
          <*> maxSize

      -- Stretch to fit width of definite available space
      margin = toAbsOrZero style.margin parentSize.width
      availableSpaceSize =
        Size
          { width =
              (\px -> px - horizontalSum margin)
                <$> intoPixels availableSpace.width,
            height = Nothing
          }
   in maybeOr
        <$> knownDims
        <*> (maybeOr <$> minMaxSize <*> (maybeOr <$> clampedSize <*> availableSpaceSize))
