{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import qualified Data.Text    as T
import           Elm          (Options, Spec (Spec), defaultOptions,
                               fieldLabelModifier, specsToDir,
                               toElmDecoderSourceWith, toElmTypeSourceWith)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Get, JSON, QueryParam)
import           Servant.Elm  (ElmOptions (..), ElmType, Proxy (Proxy),
                               defElmImports, defElmOptions,
                               generateElmForAPIWith)

data GifData = GifData
  { image_url :: String
  } deriving (Show, Eq, Generic)

data Gif = Gif
  { _data :: GifData
  } deriving (Show, Eq, Generic)

instance ElmType GifData
instance ElmType Gif

stripUnderscore :: T.Text -> T.Text
stripUnderscore field =
  if T.head field == '_' then
    T.tail field
  else
    field

options :: Elm.Options
options =
  Elm.defaultOptions
    { Elm.fieldLabelModifier = stripUnderscore }

myElmOpts :: ElmOptions
myElmOpts =
  defElmOptions
    { urlPrefix =
        "http://api.giphy.com/v1/gifs"
    , elmExportOptions =
        options
    }

type GiphyApi = "random" :> QueryParam "api_key" String :> QueryParam "tag" String :> Get '[JSON] Gif

giphySpec :: Spec
giphySpec = Spec ["Generated", "GiphyApi"]
                 (defElmImports
                  : toElmTypeSourceWith    options (Proxy :: Proxy Gif)
                  : toElmTypeSourceWith    options (Proxy :: Proxy GifData)
                  : toElmDecoderSourceWith options (Proxy :: Proxy Gif)
                  : toElmDecoderSourceWith options (Proxy :: Proxy GifData)
                  : generateElmForAPIWith
                      myElmOpts
                      (Proxy :: Proxy GiphyApi))

main :: IO ()
main = specsToDir [giphySpec] "elm"
