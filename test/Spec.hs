import Block
import Dimension
import Flex
import Geometry
import Style
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Block.nodeSize" $ do
    it "returns a size of zero for a default style" $ do
      nodeSize
        (Node defaultStyle [])
        (pure Nothing)
        (pure Nothing)
        (pure MaxContent)
        `shouldBe` Size 0 0

    it "calculates the size of a container with nested nodes" $ do
      nodeSize
        ( Node
            defaultStyle
            [ Node
                defaultStyle
                [ Node defaultStyle {Style.minSize = pure (Fixed $ Points 50)} [],
                  Node defaultStyle {Style.minSize = pure (Fixed $ Points 50)} []
                ],
              Node defaultStyle {Style.minSize = pure (Fixed $ Points 100)} []
            ]
        )
        (pure Nothing)
        (pure Nothing)
        (pure MaxContent)
        `shouldBe` Size 200 200
