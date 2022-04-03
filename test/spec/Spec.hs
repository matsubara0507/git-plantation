import           RIO

import qualified Spec.Git.Plantation.Score
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = defaultMain =<< spec

spec :: IO TestTree
spec = testGroup "Git.Plantation" <$> sequence
  [ testSpec "Git.Plantation.Score" Spec.Git.Plantation.Score.spec
  ]
