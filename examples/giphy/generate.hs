{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Proxy
import           Elm          (Spec (Spec), ToElmType, specsToDir)
import           GHC.Generics (Generic)
import           Servant      ((:<|>), (:>), Capture, Get, JSON, Post, ReqBody, QueryParam)
import           Servant.Elm  (ElmOptions (..), defElmImports, defElmOptions,
                               generateElmForAPIWith)


data GifData = GifData
  { image_url :: String
  } deriving (Show, Eq, Generic)

data Gif = Gif
  { _data :: GifData
  } deriving (Show, Eq, Generic)

instance ToElmType GifData
instance ToElmType Gif

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix =  "http://api.giphy.com/v1/gifs" }

type GiphyApi = "random" :> QueryParam "api_key" String :> QueryParam "tag" String :> Get '[JSON] Gif

giphySpec :: Spec
giphySpec = Spec ["Generated", "GiphyApi"]
                 (defElmImports
                  : generateElmForAPIWith
                      myElmOpts
                      (Proxy :: Proxy GiphyApi))

main :: IO ()
main = specsToDir [giphySpec] "elm"
