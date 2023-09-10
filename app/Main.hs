module Main (main) where

import Block
import Dimension
import Flex
import Style

main :: IO ()
main =
  print $
    mkLayout
      (Node defaultStyle {Style.minSize = pure (Fixed $ Points 200)} [])
      (pure MaxContent)
