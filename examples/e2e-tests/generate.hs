{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import           Servant.API  ((:<|>), (:>), Capture, Get, GetNoContent, JSON,
                               Post, QueryParam, ReqBody)
import           Servant.Elm  (DefineElm (DefineElm), ElmOptions(..), Proxy (Proxy),
                               UrlPrefix (Static), defaultOptions, defElmImports,
                               defElmOptions, deriveBoth, generateElmModuleWith)

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Static "https://httpbin.org" }


data MessageBody = MessageBody
  { message :: String }

data QueryArgs = QueryArgs
  { q :: String }

data Response  = Response
  { origin :: String }

data ResponseWithJson  = ResponseWithJson
  { json :: MessageBody }

data ResponseWithArgs  = ResponseWithArgs
  { args :: QueryArgs }

concat <$> mapM
  (deriveBoth defaultOptions)
  [''MessageBody, ''QueryArgs, ''Response, ''ResponseWithJson, ''ResponseWithArgs]


type Api
     = "ip"
    :> Get '[JSON] Response
  :<|> "status"
    :> "204"
    :> GetNoContent '[JSON] ()
  :<|> "post"
    :> ReqBody '[JSON] MessageBody
    :> Post '[JSON] ResponseWithJson
  :<|> "get"
    :> QueryParam "q" String
    :> Get '[JSON] ResponseWithArgs
  :<|> Capture "path" String
    :> Get '[JSON] Response


main :: IO ()
main =
  generateElmModuleWith
    myElmOpts
    [ "Generated"
    , "Api"
    ]
    defElmImports
    "elm"
    [ DefineElm (Proxy :: Proxy MessageBody)
    , DefineElm (Proxy :: Proxy QueryArgs)
    , DefineElm (Proxy :: Proxy Response)
    , DefineElm (Proxy :: Proxy ResponseWithJson)
    , DefineElm (Proxy :: Proxy ResponseWithArgs)
    ]
    (Proxy :: Proxy Api)
