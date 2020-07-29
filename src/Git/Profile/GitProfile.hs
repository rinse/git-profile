module Git.Profile.GitProfile where

import           Control.Monad.IO.Class
import qualified Data.Map               as M
import qualified Data.Text              as T
import           Data.Yaml.Aeson        (decodeFileThrow)
import           Turtle                 (decodeString, encodeString, home,
                                         (</>))


{- $setup
>>> :set -XOverloadedStrings
>>> import qualified Data.ByteString as B
>>> import qualified Data.Yaml       as Y
>>> :{
    lines :: [B.ByteString] -> B.ByteString
    lines = foldr (\x -> (x `B.append` "\n" `B.append`)) ""
    :}
-}

{- |Represents the .gitprofile file format.
>>> :{
    let input = lines [ "profile1:"
                      , "  user:"
                      , "    name: author"
                      , "    email: email"
                      , "profile2:"
                      , "  user:"
                      , "    name: author"
                      , "    email: email"
                      ]
     in Y.decodeEither' input :: Either Y.ParseException GitProfile
:}
Right (fromList [("profile1",fromList [("user",fromList [("email","email"),("name","author")])]),("profile2",fromList [("user",fromList [("email","email"),("name","author")])])])
-}
type GitProfile = M.Map ProfileName GitConfigs
type ProfileName = T.Text

{- |Represents each config of GitProfile.
>>> :{
    let input = lines [ "user:"
                      , "  name: author"
                      , "  email: email@sample.com"
                      ]
     in Y.decodeEither' input :: Either Y.ParseException GitConfigs
:}
Right (fromList [("user",fromList [("email","email@sample.com"),("name","author")])])
-}
type GitConfigs = M.Map Category (M.Map Key Value)
type Category = T.Text
type Key = T.Text
type Value = T.Text

-- |Obtains the default .gitprofile.
gitProfile :: MonadIO m => m GitProfile
gitProfile = do
    h <- home
    decodeFileThrow . encodeString $ h </> decodeString ".gitprofile"
