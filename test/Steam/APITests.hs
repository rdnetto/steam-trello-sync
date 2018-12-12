module Steam.APITests where

import BasicPrelude
import Data.Aeson (FromJSON, eitherDecode)
import Data.Time.Clock (secondsToDiffTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, assertEqual, assertFailure)

import Steam.API


tests :: TestTree
tests = testGroup "Steam.API" [
        responseParsing
    ]

responseParsing :: TestTree
responseParsing = testCase "Parse GameResponse" assertion where
    assertion = assertDecodesAs expected json
    json = "{\"response\":{\"game_count\":266,\"games\":[{\"appid\":6880,\"name\":\"Just Cause\",\"playtime_forever\":1,\"img_icon_url\":\"aeaef41cac61d12ef93cf5eddc86859b1f0c73fa\",\"img_logo_url\":\"60d726f5e078a6da168414cf45b8619973f6d69c\",\"has_community_visible_stats\":true}]}}"
    expected = GamesResponse 266 [
            GameInfo
                (AppId 6880)
                "Just Cause"
                (secondsToDiffTime 60)
                (secondsToDiffTime 0)
                (SteamImageHash "aeaef41cac61d12ef93cf5eddc86859b1f0c73fa")
                (SteamImageHash "60d726f5e078a6da168414cf45b8619973f6d69c")
        ]

-- Helper function for testing JSON parsing
assertDecodesAs :: (Eq a, Show a, FromJSON a) => a -> LByteString -> Assertion
assertDecodesAs expected json = f $ eitherDecode json where
    f (Left err)    = assertFailure err
    f (Right value) = assertEqual "Values did not match" expected value
