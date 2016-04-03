{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import           GHC.Generics (Generic)
import           Servant.API  ((:>), Get, JSON, QueryParam)
import           Servant.Elm  (ElmOptions (..), ElmType, Proxy (Proxy),
                               Spec (Spec), defElmImports, defElmOptions,
                               generateElmForAPIWith, specsToDir)


data GifData = GifData
  { image_url :: String
  } deriving (Show, Eq, Generic)

data Gif = Gif
  { _data :: GifData
  } deriving (Show, Eq, Generic)

instance ElmType GifData
instance ElmType Gif

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
