# Flex
[![CI](https://github.com/matthunz/flex/actions/workflows/haskell.yml/badge.svg)](https://github.com/matthunz/flex/actions/workflows/haskell.yml)

Layout engine for user interfaces that supports the CSS block and flexbox (TODO) algorithms.
Based on [taffy](https://github.com/DioxusLabs/taffy) and [yoga](https://yogalayout.com).
```hs
layoutNode :: Node -> Size AvailableSpace -> LayoutNode
```

## Examples
```hs
import Block
import Dimension
import Flex
import Style

main :: IO ()
main =
  print $
    layoutNode
      (Node defaultStyle {Style.minSize = pure (Fixed $ Points 200)} [])
      (pure MaxContent)

```
