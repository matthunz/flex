module Dimension
  ( -- * Fixed Dimension
    FixedDimension (..),
    fixedToAbsOrZero,

    -- * Dimension
    Dimension (..),
    fMaybeToAbs,
    toAbsOrZero,
  )
where

import Data.Maybe (fromMaybe)

-- | A fixed dimension of points or percentage.
data FixedDimension = Points Float | Percent Float deriving (Show)

-- | Resolve a relative fixed dimension to its value in points.
maybeToAbs :: Maybe FixedDimension -> Maybe Float -> Maybe Float
maybeToAbs (Just (Points points)) _ = Just points
maybeToAbs (Just (Percent percent)) (Just parent) = Just $ percent * parent
maybeToAbs _ _ = Nothing

data Dimension = Fixed FixedDimension | Auto deriving (Show)

toFixed :: Dimension -> Maybe FixedDimension
toFixed (Fixed dim) = Just dim
toFixed Auto = Nothing

fMaybeFixedToAbs ::
  (Applicative f) =>
  f (Maybe FixedDimension) ->
  f (Maybe Float) ->
  f (Maybe Float)
fMaybeFixedToAbs dimensionSize parentSize = maybeToAbs <$> dimensionSize <*> parentSize

fMaybeToAbs :: (Applicative f) => f Dimension -> f (Maybe Float) -> f (Maybe Float)
fMaybeToAbs size = fMaybeFixedToAbs (fmap toFixed size)

fixedToAbsOrZero :: (Functor f) => f (Maybe FixedDimension) -> Maybe Float -> f Float
fixedToAbsOrZero rect parent = fmap (\dim -> fromMaybe 0 (maybeToAbs dim parent)) rect

toAbsOrZero :: (Functor f) => f Dimension -> Maybe Float -> f Float
toAbsOrZero rect = fixedToAbsOrZero (fmap toFixed rect)
