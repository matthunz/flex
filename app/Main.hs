module Main (main) where

import Lib

main :: IO ()
main =
  print $
    layout
      ( Node
          (defaultStyle {direction = Column, size = Size (Percent 1) (Percent 1)})
          [ Node (defaultStyle {size = Size (Percent 1) (Percent 1)}) [],
            Node (defaultStyle {size = Size (Percent 1) (Percent 1)}) []
          ]
      )
      (Size 100 100)
