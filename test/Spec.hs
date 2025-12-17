import Test.Hspec
import qualified KnowledgeHook.Properties as Props

main :: IO ()
main = hspec $ do
  Props.spec
