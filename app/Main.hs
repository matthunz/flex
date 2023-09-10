module Main (main) where

import Dimension
import Flex
import Style
import Flex

main :: IO ()
main =
  print $
    mkLayout
      (defaultStyle {Style.minSize = pure (Fixed $ Points 100)})
      (pure MaxContent)
