# Flex
Layout engine for user interfaces that supports the CSS block and flexbox (TODO) algorithms.
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
