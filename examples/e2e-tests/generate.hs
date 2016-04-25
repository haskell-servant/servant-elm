{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

import           GHC.Generics (Generic)
import           Servant.API  ((:<|>), (:>), Capture, Get, GetNoContent, JSON,
                               NoContent, Post, QueryParam, ReqBody)
import           Servant.Elm  (ElmOptions (..), ElmType, Proxy (Proxy),
                               Spec (Spec), defElmImports, defElmOptions,
                               generateElmForAPIWith, specsToDir)


myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix =  "https://httpbin.org" }


data OriginIp = OriginIp
  { origin :: String }
  deriving (Generic, ElmType)

data MessageBody = MessageBody
  { message :: String }
  deriving (Generic, ElmType)

data MessageResponse = MessageResponse
  { json :: MessageBody }
  deriving (Generic, ElmType)

data QueryArgsResponse = QueryArgsResponse
  { args :: QueryArgs }
  deriving (Generic, ElmType)

data QueryArgs = QueryArgs
  { q :: String }
  deriving (Generic, ElmType)


type Api
     = "ip"
    :> Get '[JSON] OriginIp
  :<|> "status"
    :> "204"
    :> GetNoContent '[JSON] NoContent
  :<|> "post"
    :> ReqBody '[JSON] MessageBody
    :> Post '[JSON] MessageResponse
  :<|> "get"
    :> QueryParam "q" String
    :> Get '[JSON] QueryArgsResponse
  :<|> Capture "path" String
    :> Get '[JSON] OriginIp


spec :: Spec
spec =
  Spec ["Generated", "Api"]
    (defElmImports
     : generateElmForAPIWith
         myElmOpts
         (Proxy :: Proxy Api))


main :: IO ()
main = specsToDir [spec] "elm"
