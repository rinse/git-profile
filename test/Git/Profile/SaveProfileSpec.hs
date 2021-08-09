{-# LANGUAGE OverloadedStrings #-}
module Git.Profile.SaveProfileSpec where

import           Git.Profile.SaveProfile (readConfigName)
import           RIO
import           Test.Hspec

spec :: Spec
spec = do
    describe "readConfigName" $ do
        it "splits 'Section.Key' into a section and a key." $ do
            let actual = readConfigName "Section.Key"
            actual `shouldBe` Just ("Section", "Key")
        it "regards a substring following the first dot as a key even if the substring contains dots." $ do
            let actual = readConfigName "Section.Key.Something.Something"
            actual `shouldBe` Just ("Section", "Key.Something.Something")
        it "returns Nothing when the given config name do not contain a dot." $ do
            let actual = readConfigName "user"
            actual `shouldBe` Nothing
        it "returns Nothing for an empty string." $ do
            let actual = readConfigName ""
            actual `shouldBe` Nothing
        it "returns Nothing when the config name has no key." $ do
            let actual = readConfigName "Section."
            actual `shouldBe` Nothing
        it "returns Nothing when the config name has no section." $ do
            let actual = readConfigName ".Key"
            actual `shouldBe` Nothing
        it "returns Nothing when the config name has no section nor no key." $ do
            let actual = readConfigName "."
            actual `shouldBe` Nothing
