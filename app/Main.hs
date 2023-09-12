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
  defaultMain
    [ bgroup
        "fib"
        [ bench "1" $
            whnf
              app
              ( replicate
                  100000
                  ( Node
                      defaultStyle
                        { Style.minSize = pure (Fixed $ Percent 0.5)
                        }
                      []
                  )
              )
        ]
    ]
