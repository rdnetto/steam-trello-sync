import BasicPrelude
import Test.Tasty (defaultMain, testGroup)

import qualified Steam.APITests


main :: IO ()
main = defaultMain $ testGroup "Tests" [
        Steam.APITests.tests
    ]
