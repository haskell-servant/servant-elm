{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmEncoderSource, toElmTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:<|>), (:>), Capture, Get, GetNoContent, JSON,
                               NoContent, Post, QueryParam, ReqBody)
import           Servant.Elm  (ElmOptions (..), ElmType, Proxy (Proxy),
                               UrlPrefix (Static), defElmImports, defElmOptions,
                               generateElmForAPIWith)


myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Static "https://httpbin.org" }


data MessageBody = MessageBody
  { message :: String }
  deriving (Generic, ElmType)

data QueryArgs = QueryArgs
  { q :: String }
  deriving (Generic, ElmType)

data Response  = Response
  { origin :: String }
  deriving (Generic, ElmType)

data ResponseWithJson  = ResponseWithJson
  { json :: MessageBody }
  deriving (Generic, ElmType)

data ResponseWithArgs  = ResponseWithArgs
  { args :: QueryArgs }
  deriving (Generic, ElmType)


type Api
     = "ip"
    :> Get '[JSON] Response
  :<|> "status"
    :> "204"
    :> GetNoContent '[JSON] NoContent
  :<|> "post"
    :> ReqBody '[JSON] MessageBody
    :> Post '[JSON] ResponseWithJson
  :<|> "get"
    :> QueryParam "q" String
    :> Get '[JSON] ResponseWithArgs
  :<|> Capture "path" String
    :> Get '[JSON] Response


spec :: Spec
spec =
  Spec ["Generated", "Api"]
    (defElmImports
     : toElmTypeSource    (Proxy :: Proxy Response)
     : toElmDecoderSource (Proxy :: Proxy Response)
     : toElmTypeSource    (Proxy :: Proxy NoContent)
     : toElmTypeSource    (Proxy :: Proxy MessageBody)
     : toElmEncoderSource (Proxy :: Proxy MessageBody)
     : toElmDecoderSource (Proxy :: Proxy MessageBody)
     : toElmTypeSource    (Proxy :: Proxy ResponseWithJson)
     : toElmDecoderSource (Proxy :: Proxy ResponseWithJson)
     : toElmTypeSource    (Proxy :: Proxy QueryArgs)
     : toElmDecoderSource (Proxy :: Proxy QueryArgs)
     : toElmTypeSource    (Proxy :: Proxy ResponseWithArgs)
     : toElmDecoderSource (Proxy :: Proxy ResponseWithArgs)
     : generateElmForAPIWith
         myElmOpts
         (Proxy :: Proxy Api))


main :: IO ()
main = specsToDir [spec] "elm"
