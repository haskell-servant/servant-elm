{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import           GHC.Generics (Generic)
import           Servant.API  ((:<|>), (:>), Get, GetNoContent, JSON, NoContent,
                               Post, ReqBody)
import           Servant.Elm  (ElmOptions (..), ElmType, Proxy (Proxy),
                               Spec (Spec), defElmImports, defElmOptions,
                               generateElmForAPIWith, specsToDir)


myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix =  "https://httpbin.org" }


data OriginIp = OriginIp
  { origin :: String }
  deriving Generic

instance ElmType OriginIp

data MessageBody = MessageBody
  { message :: String }
  deriving Generic

instance ElmType MessageBody

data MessageResponse = MessageResponse
  { json :: MessageBody }
  deriving Generic

instance ElmType MessageResponse


type Api
     = "ip"
    :> Get '[JSON] OriginIp
  :<|> "status"
    :> "204"
    :> GetNoContent '[JSON] NoContent
  :<|> "post"
    :> ReqBody '[JSON] MessageBody
    :> Post '[JSON] MessageResponse


spec :: Spec
spec =
  Spec ["Generated", "Api"]
    (defElmImports
     : generateElmForAPIWith
         myElmOpts
         (Proxy :: Proxy Api))


main :: IO ()
main = specsToDir [spec] "elm"
