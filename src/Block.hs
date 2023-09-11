{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Block
  ( -- * Sizing
    -- $sizing
    nodeSize,

    -- * Layout
    -- $layout
    layoutNode,
    layoutNodeInner,
  )
where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Dimension (fMaybeToAbs, fixedToAbsOrZero, toAbsOrZero)
import Flex
  ( AvailableSpace (MinContent, Pixels),
    Layout (Layout, order, size),
    LayoutNode (..),
    Node (nodes, style), toPx,
  )
import Geometry (Rect (top), Size (..), horizontalSum)
import Style
  ( Display (None),
    Style (border, display, margin, maxSize, minSize, padding, size),
  )

-- $sizing
--
-- Sizing functions perform a minimal block layout to calculate a container size.

-- | Calculate the size of a container.
nodeSize ::
  Node ->
  Size (Maybe Float) ->
  Size (Maybe Float) ->
  Size AvailableSpace ->
  Size Float
nodeSize node knownDims parentSize availableSpace =
  let (Size width height, _) = nodeLayout node knownDims parentSize availableSpace
   in Size {width = width, height = fromMaybe height knownDims.height}

-- $layout
--
-- Layout functions perform complete layout using the block display algorithm.

-- | Calculate the layout of a node tree.
layoutNode :: Node -> Size AvailableSpace -> LayoutNode
layoutNode node availableSpace =
  layoutNodeInner
    node
    (pure Nothing)
    (fmap toPx availableSpace)
    availableSpace

-- | Calculate the layout of a node tree with its known dimensions,
-- | parent size and available space.
layoutNodeInner ::
  Node ->
  Size (Maybe Float) ->
  Size (Maybe Float) ->
  Size AvailableSpace ->
  LayoutNode
layoutNodeInner node knownDims parentSize availableSpace =
  let styleKnownDims = layoutSize node.style knownDims parentSize availableSpace
      (size, children) = nodeLayout node styleKnownDims parentSize availableSpace
   in LayoutNode {layout = Layout {order = 0, size = size}, children = children}

-- Internal

data BlockItem = BlockItem
  { order :: Int,
    size :: Size (Maybe Float),
    minSize :: Size (Maybe Float),
    maxSize :: Size (Maybe Float),
    node :: Node
  }

-- | Convert a list of nodes into block items using their parent's size.
blockItems :: [Node] -> Size (Maybe Float) -> [BlockItem]
blockItems nodes parentSize =
  filter (\node -> node.style.display /= None) nodes
    & zipWith
      ( \order node ->
          BlockItem
            { order = order,
              size = fMaybeToAbs node.style.size parentSize,
              minSize = fMaybeToAbs node.style.minSize parentSize,
              maxSize = fMaybeToAbs node.style.maxSize parentSize,
              node = node
            }
      )
      [0 ..]

nodeLayout ::
  Node ->
  Size (Maybe Float) ->
  Size (Maybe Float) ->
  Size AvailableSpace ->
  (Size Float, [LayoutNode])
nodeLayout node knownDims parentSize availableSpace =
  let -- Convert child nodes into block items
      items = blockItems node.nodes knownDims

      -- Calculate the width
      width = layoutWidth node.style items parentSize availableSpace

      -- Lazily resolve the height when it's unknown
      toAbsWithWidth field =
        fixedToAbsOrZero (Just <$> field node.style) parentSize.width
      border = toAbsWithWidth Style.border
      padding = toAbsWithWidth Style.padding
      inset = (+) <$> border <*> padding
      (intrinsicHeight, children) = layoutNodeHeight node.style items width inset

      minSize = fMaybeToAbs node.style.minSize parentSize
      maxSize = fMaybeToAbs node.style.maxSize parentSize
      innerHeight = clamp intrinsicHeight minSize.height maxSize.height

      height = fromMaybe innerHeight knownDims.height
   in (Size width height, children)

mkContentWidth :: [BlockItem] -> AvailableSpace -> Float
mkContentWidth items availableSpace =
  sum $
    map
      ( \item ->
          width $
            nodeSize
              item.node
              (maybeClamp <$> item.size <*> item.minSize <*> item.maxSize)
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
                <$> toPx availableSpace.width,
            height = Nothing
          }
   in maybeOr
        <$> knownDims
        <*> (maybeOr <$> minMaxSize <*> (maybeOr <$> clampedSize <*> availableSpaceSize))

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

layoutNodeHeight :: Style -> [BlockItem] -> Float -> Rect Float -> (Float, [LayoutNode])
layoutNodeHeight style items outerWidth inset =
  let border = fixedToAbsOrZero (Just <$> style.border) (Just outerWidth)
      padding = fixedToAbsOrZero (Just <$> style.border) (Just outerWidth)
      resolvedInset = (+) <$> border <*> padding
   in layoutNodeFlow items outerWidth inset resolvedInset

layoutNodeFlow :: [BlockItem] -> Float -> Rect Float -> Rect Float -> (Float, [LayoutNode])
layoutNodeFlow items outerWidth inset resolvedInset =
  let innerWidth = outerWidth - horizontalSum inset
      parentSize = Size {width = Just outerWidth, height = Nothing}
      availableSpace = Size {width = Pixels innerWidth, height = MinContent}

      f :: BlockItem -> (Float, [LayoutNode]) -> (Float, [LayoutNode])
      f item (offset, acc) =
        let (itemSize, children) = nodeLayout item.node (pure Nothing) parentSize availableSpace
            layout = Layout {order = item.order, size = itemSize}
         in (offset + itemSize.height, LayoutNode layout children : acc)
   in foldr f (resolvedInset.top, []) items

-- Utils

maybeClamp :: Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float
maybeClamp val minVal maxVal = (\v -> clamp v minVal maxVal) <$> val

clamp :: Float -> Maybe Float -> Maybe Float -> Float
clamp val minValCell maxValCell =
  let clamped = case minValCell of
        Just minVal -> max val minVal
        Nothing -> val
   in case maxValCell of
        Just maxVal -> min clamped maxVal
        Nothing -> clamped

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just a) _ = Just a
maybeOr Nothing b = b
