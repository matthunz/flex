{-# LANGUAGE DuplicateRecordFields #-}

module Flex where

import Geometry
import Style

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

data Node = Node
  { style :: Style,
    nodes :: [Node]
  }
  deriving (Show)

data Layout = Layout
  { order :: Int,
    size :: Size Float
  }
  deriving (Show)

data LayoutNode = LayoutNode
  { layout :: Layout,
    children :: [LayoutNode]
  }
  deriving (Show)

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just a) _ = Just a
maybeOr Nothing b = b

data AvailableSpace = Pixels Float | MinContent | MaxContent

intoPixels :: AvailableSpace -> Maybe Float
intoPixels (Pixels px) = Just px
intoPixels _ = Nothing
