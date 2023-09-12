{-# LANGUAGE DuplicateRecordFields #-}

module Flex
  ( Node (..),
    Layout (..),
    LayoutNode (..),
    AvailableSpace (..),
    toPx,
  )
where

import Geometry
import Style

data Node = Node
  { style :: Style,
    nodes :: [Node]
  }
  deriving (Show)

data Layout = Layout
  { order :: Int,
    position :: Point Float,
    size :: Size Float
  }
  deriving (Show)

data LayoutNode = LayoutNode
  { layout :: Layout,
    children :: [LayoutNode]
  }
  deriving (Show)

data AvailableSpace = Pixels Float | MinContent | MaxContent

toPx :: AvailableSpace -> Maybe Float
toPx (Pixels px) = Just px
toPx _ = Nothing
