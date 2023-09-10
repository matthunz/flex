module Main (main) where

import Dimension
import Flex
import Style

main :: IO ()
main =
  print $
    mkNodeLayout
      (Node defaultStyle {Style.minSize = pure (Fixed $ Points 100)} [])
      (pure (Just 100))
      (pure MaxContent)
