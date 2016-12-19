{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.Monoid ((<>))
import Data.Text
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody)
import qualified Network.Wai.Handler.Warp as Warp
import Servant

data BBUser = BBUser { _bbuUsername :: Text }
makeLenses ''BBUser
instance FromJSON BBUser where
  parseJSON (Object o) = BBUser <$> o .: "username"
  parseJSON _ = mzero

data BBAuthor = BBAuthor { _bbaUser :: BBUser }
makeLenses ''BBAuthor
instance FromJSON BBAuthor where
  parseJSON (Object o) = BBAuthor <$> o .: "user"
  parseJSON _ = mzero

data BBTarget = BBTarget { _bbtAuthor :: BBAuthor, _bbtHash :: Text }
makeLenses ''BBTarget
instance FromJSON BBTarget where
  parseJSON (Object o) = BBTarget <$> o .: "author" <*> o .: "hash"
  parseJSON _ = mzero

data BBNew = BBNew { _bbnTarget :: BBTarget, _bbnName :: Text }
makeLenses ''BBNew
instance FromJSON BBNew where
  parseJSON (Object o) = BBNew <$> o .: "target" <*> o .: "name"
  parseJSON _ = mzero

data BBChange = BBChange { _bbcNew :: BBNew }
makeLenses ''BBChange
instance FromJSON BBChange where
  parseJSON (Object o) = BBChange <$> o .: "new"
  parseJSON _ = mzero

data BBPush = BBPush { _bbpChanges :: [BBChange] }
makeLenses ''BBPush
instance FromJSON BBPush where
  parseJSON (Object o) = BBPush <$> o .: "changes"
  parseJSON _ = mzero

data BBData = BBData { _bbdPush :: BBPush }
makeLenses ''BBData
instance FromJSON BBData where
  parseJSON (Object o) = BBData <$> o .: "push"
  parseJSON _ = mzero

buildURL :: Text -> Text -> Text
buildURL a p = "http://localhost:5000/api/add?author=" <> a <> "&patch=" <> p

type BuildApi =
       "check" :> Get '[JSON] Text
  :<|> "build" :> ReqBody '[JSON] BBData :> Post '[JSON] Text

buildApi :: Proxy BuildApi
buildApi = Proxy

server :: Server BuildApi
server = checkOK :<|> receiveBuild

checkOK :: Handler Text
checkOK = return "OK"

receiveBuild :: BBData -> Handler Text
receiveBuild bb =
  if b == "develop"
  then do
    u <- parseRequest (unpack $ buildURL a h)
    r <- httpLBS u
    return $ pack $ BS.unpack $ getResponseBody r
  else return $ pack "Not develop branch; Skipped"
  where
    n = bbdPush . bbpChanges . ix 0 . bbcNew
    t = n . bbnTarget
    b = bb ^. n . bbnName
    a = bb ^. t . bbtAuthor . bbaUser . bbuUsername
    h = bb ^. t . bbtHash

main :: IO ()
main = Warp.run 8080 $ serve buildApi server
