{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.GitProfileSpec (spec) where

import qualified Data.ByteString        as B
import qualified Data.Map               as M
import qualified Data.Yaml.Aeson        as Y
import           Git.Profile.GitProfile
import           RIO
import           Test.Hspec

spec :: Spec
spec = do
    describe "GitConfigs" $ do
        it "stands for configurations of git" $ do
            let Right actual = Y.decodeEither' gitConfigsSampleS
            actual `shouldBe` gitConfigsSampleG
    describe "GitProfile" $ do
        it "stands for a collection of configurations of git" $ do
            let Right actual = Y.decodeEither' gitProfileSampleS
            actual `shouldBe` gitProfileSampleG

gitConfigsSampleS :: B.ByteString
gitConfigsSampleS = unlinesBS
    [ "user:"
    , "  name: author"
    , "  email: email@example.com"
    ]

gitConfigsSampleG :: GitConfigs
gitConfigsSampleG = M.fromList
    [ ( "user"
      , M.fromList
        [ ("name", "author")
        , ("email", "email@example.com")
        ]
      )
    ]

gitProfileSampleS :: B.ByteString
gitProfileSampleS = unlinesBS
    [ "profile1:"
    , "  user:"
    , "    name: author"
    , "    email: email"
    , "profile2:"
    , "  user:"
    , "    name: author"
    , "    email: email"
    ]

gitProfileSampleG :: GitProfile
gitProfileSampleG = M.fromList
    [ ( "profile1"
      , M.fromList
        [ ( "user"
          , M.fromList
            [ ("name", "author")
            , ("email", "email")
            ]
          )
        ]
      )
    , ( "profile2"
      , M.fromList
        [ ( "user"
          , M.fromList
            [ ("name", "author")
            , ("email", "email")
            ]
          )
        ]
      )
    ]

unlinesBS :: [B.ByteString] -> B.ByteString
unlinesBS = foldr (\x -> (x `B.append` "\n" `B.append`)) ""
