{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geometry
  ( Point (..),
    Size (..),
    Rect (..),
    horizontalSum,
  )
where

data Point t = Point
  { x :: t,
    y :: t
  }
  deriving (Show)

data Size t = Size
  { width :: t,
    height :: t
  }
  deriving (Eq, Show)

instance Functor Size where
  fmap f size = Size {width = f size.width, height = f size.height}

instance Applicative Size where
  pure a = Size {width = a, height = a}

  Size {width = f, height = g} <*> size =
    Size {width = f size.width, height = g size.height}

data Rect t = Rect
  { top :: t,
    left :: t,
    bottom :: t,
    right :: t
  }
  deriving (Show)

instance Functor Rect where
  fmap f rect =
    Rect
      { top = f rect.top,
        left = f rect.left,
        bottom = f rect.bottom,
        right = f rect.right
      }

instance Applicative Rect where
  pure a = Rect a a a a

  Rect {top = t, left = l, bottom = b, right = r} <*> rect =
    Rect
      { top = t rect.top,
        left = l rect.left,
        bottom = b rect.bottom,
        right = r rect.right
      }

horizontalSum :: Rect Float -> Float
horizontalSum rect = rect.left + rect.right