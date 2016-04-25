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
     : generateElmForAPIWith
         myElmOpts
         (Proxy :: Proxy Api))


main :: IO ()
main = specsToDir [spec] "elm"
