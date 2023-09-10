{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Block where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
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
   in mkNodeLayout node styleKnownDims parentSize availableSpace

mkNodeLayout :: Node -> Size (Maybe Float) -> Size (Maybe Float) -> Size AvailableSpace -> Size Float
mkNodeLayout node knownDims parentSize availableSpace =
  let items = mkItems node.nodes knownDims
      contentWidth = mkContentWidth items availableSpace.width
      minSize = fMaybeToAbs node.style.minSize parentSize
      maxSize = fMaybeToAbs node.style.maxSize parentSize
      minOuterWidth = case minSize.width of
        Just m -> max m contentWidth
        Nothing -> contentWidth
      outerWidth = case maxSize.width of
        Just m -> min m minOuterWidth
        Nothing -> minOuterWidth
   in case knownDims.height of
        Just height -> Size {width = outerWidth, height = height}
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
            nodeSize
              item.node
              (pure Nothing)
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

flowLayout :: [BlockItem] -> Float -> Rect Float -> Rect Float -> (Float, [Layout])
flowLayout items outerWidth inset resolvedInset =
  let innerWidth = outerWidth - horizontalSum inset
      parentSize = Size {width = Just outerWidth, height = Nothing}
      availableSpace = Size {width = Pixels innerWidth, height = MinContent}

      f :: BlockItem -> (Float, [Layout]) -> (Float, [Layout])
      f item (offset, acc) =
        let itemSize = nodeSize item.node (pure Nothing) parentSize availableSpace
         in (offset + itemSize.height, acc ++ [Layout {order = item.order, size = itemSize}])
   in foldr f (resolvedInset.top, []) items

layoutWidth ::
  Style ->
  [BlockItem] ->
  Size (Maybe Float) ->
  Size AvailableSpace ->
  Float
layoutWidth style items parentSize availableSpace =
  let contentWidth = mkContentWidth items availableSpace.width
      minSize = fMaybeToAbs style.minSize parentSize
      maxSize = fMaybeToAbs style.maxSize parentSize
      minOuterWidth = case minSize.width of
        Just m -> max m contentWidth
        Nothing -> contentWidth
      outerWidth = case maxSize.width of
        Just m -> min m minOuterWidth
        Nothing -> minOuterWidth
   in outerWidth

layoutHeight :: Style -> [BlockItem] -> Float -> Rect Float -> (Float, [Layout])
layoutHeight style items outerWidth inset =
  let border = fixedToAbsOrZero (Just <$> style.border) (Just outerWidth)
      padding = fixedToAbsOrZero (Just <$> style.border) (Just outerWidth)
      resolvedInset = (+) <$> border <*> padding
   in flowLayout items outerWidth inset resolvedInset

-- | Calculate the size of a container.
nodeSize ::
  Node ->
  Size (Maybe Float) ->
  Size (Maybe Float) ->
  Size AvailableSpace ->
  Size Float
nodeSize node knownDims parentSize availableSpace =
  let -- Convert child nodes into block items
      items = mkItems node.nodes knownDims

      -- Calculate the width
      width = layoutWidth node.style items parentSize availableSpace

      -- Lazily resolve the height when it's unknown
      toAbsWithWidth field =
        fixedToAbsOrZero (Just <$> field node.style) parentSize.width
      border = toAbsWithWidth Style.border
      padding = toAbsWithWidth Style.padding
      inset = (+) <$> border <*> padding
      (resolvedHeight, _) = layoutHeight node.style items width inset

      -- Calculate the height
      height = fromMaybe resolvedHeight knownDims.height
   in Size width height
