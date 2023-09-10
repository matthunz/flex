{-# LANGUAGE DuplicateRecordFields #-}

module Flex where

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

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just a) _ = Just a
maybeOr Nothing b = b

data AvailableSpace = Pixels Float | MinContent | MaxContent

intoPixels :: AvailableSpace -> Maybe Float
intoPixels (Pixels px) = Just px
intoPixels _ = Nothing
