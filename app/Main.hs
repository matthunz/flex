module Main (main) where

import Block
import Criterion.Main
import Dimension
import Flex
import Style

app :: [Node] -> LayoutNode
app nodes =
  layoutNode
    (Node defaultStyle nodes)
    (pure MaxContent)

main :: IO ()
main =
  print $
    app
      [ Node defaultStyle {Style.minSize = pure $ Fixed $ Points 100} [],
        Node defaultStyle {Style.minSize = pure $ Fixed $ Points 100} []
      ]
