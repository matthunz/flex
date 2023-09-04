module Main (main) where

import Lib

main :: IO ()
main = print $ layout (Node (Style (Size (Points 100) (Percent 1))) []) (Size 100 100)
