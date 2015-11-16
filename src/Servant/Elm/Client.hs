{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Elm.Client where

import           Data.Proxy          (Proxy (Proxy))
import qualified Data.Text           as T
import           Elm                 (ToElmType, maybeToElmTypeSource,
                                      maybeToElmDecoderSource, toElmTypeName, toElmDecoderName)
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Servant.API         ((:<|>), (:>), Capture, Get, Post,
                                      QueryFlag, QueryParam, QueryParams)
import           Servant.Foreign     (ArgType (..), QueryArg (..), Segment (..),
                                      SegmentType (..))

import           Servant.Elm.Request (Request (..), addArgName, setDecoder,
                                      addDecoderDef,
                                      addTypeDef, addFnName, addFnSignature,
                                      addUrlQueryStr, addUrlSegment, defRequest,
                                      setHttpMethod)

{-
TODO:
Servant API coverage
* MatrixFlag / MatrixParam / MatrixParams
* Header (request)
* Headers (response)
* Delete / Patch / Put / Raw?
* ReqBody
* Vault / RemoteHost / IsSecure

* Generate: use toString for params
* Generate Json encoders?
* ToText stuff for captures/params?
* Option to not use elm-export
-}


elmClient :: (HasElmClient layout)
          => Proxy layout -> [Request]
elmClient p = elmClientWithRoute p defRequest


class HasElmClient layout where
  elmClientWithRoute :: Proxy layout -> Request -> [Request]


-- a :<|> b
instance (HasElmClient a, HasElmClient b) => HasElmClient (a :<|> b) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy a) result ++
    elmClientWithRoute (Proxy :: Proxy b) result


-- path :> rest
instance (KnownSymbol path, HasElmClient sublayout) => HasElmClient (path :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addFnName p . addUrlSegment segment) result)
    where p = symbolVal (Proxy :: Proxy path)
          segment = Segment (Static (T.pack p))


-- Capture name ArgType :> rest
instance (KnownSymbol capture, ToElmType a, HasElmClient sublayout)
      => HasElmClient (Capture capture a :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addTypeDef (maybeToElmTypeSource argProxy)
                         . addFnSignature (toElmTypeName argProxy)
                         . addFnName "by"
                         . addArgName argName
                         . addUrlSegment (Segment (Cap (T.pack argName)))) result)
      where argProxy = Proxy :: Proxy a
            argName = symbolVal (Proxy :: Proxy capture)


-- QueryFlag name :> rest
instance (KnownSymbol sym, HasElmClient sublayout)
      => HasElmClient (QueryFlag sym :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addArgName argName
                         . addFnSignature (toElmTypeName (Proxy :: Proxy Bool))
                         . addUrlQueryStr (QueryArg (T.pack argName) Flag)) result)
      where argName = symbolVal (Proxy :: Proxy sym)


-- QueryParam name ArgType :> rest
instance (KnownSymbol sym, ToElmType a, HasElmClient sublayout)
      => HasElmClient (QueryParams sym a :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addArgName argName
                         . addTypeDef (maybeToElmTypeSource (Proxy :: Proxy [a]))
                         . addFnSignature (toElmTypeName (Proxy :: Proxy [a]))
                         . addUrlQueryStr (QueryArg (T.pack argName) List)) result)
      where argName = symbolVal (Proxy :: Proxy sym)


-- QueryParams name ArgType :> rest
instance (KnownSymbol sym, ToElmType a, HasElmClient sublayout)
      => HasElmClient (QueryParam sym a :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addArgName argName
                         . addTypeDef (maybeToElmTypeSource (Proxy :: Proxy a))
                         . addFnSignature (toElmTypeName (Proxy :: Proxy a))
                         . addUrlQueryStr (QueryArg (T.pack argName) Normal)) result)
      where argName = symbolVal (Proxy :: Proxy sym)


-- Get '[cts] RequestType :> rest
instance {-# OVERLAPPABLE #-} (ToElmType apiRequest) => HasElmClient (Get (ct ': cts) apiRequest) where
  elmClientWithRoute Proxy request =
    [(setHttpMethod "GET"
      . addFnSignature elmTypeName
      . addTypeDef (maybeToElmTypeSource apiRequestProxy)
      . addDecoderDef (maybeToElmDecoderSource apiRequestProxy)
      . setDecoder (toElmDecoderName apiRequestProxy)) request]
    where
      apiRequestProxy = Proxy :: Proxy apiRequest
      elmTypeName = toElmTypeName apiRequestProxy


-- Get '[cts] () :> rest
instance {-# OVERLAPPING #-} HasElmClient (Get (ct ': cts) ()) where
  elmClientWithRoute Proxy request =
    [(setHttpMethod "GET"
      . addFnSignature "()"
      . setDecoder "(succeed ())") request]


-- Post '[cts] RequestType :> rest
instance {-# OVERLAPPABLE #-} (ToElmType apiRequest) => HasElmClient (Post (ct ': cts) apiRequest) where
  elmClientWithRoute Proxy request =
    [(setHttpMethod "POST"
      . addFnSignature elmTypeName
      . addTypeDef (maybeToElmTypeSource apiRequestProxy)
      . addDecoderDef (maybeToElmDecoderSource apiRequestProxy)
      . setDecoder (toElmDecoderName apiRequestProxy)) request]
    where
      apiRequestProxy = Proxy :: Proxy apiRequest
      elmTypeName = toElmTypeName apiRequestProxy


-- Post '[cts] () :> rest
instance {-# OVERLAPPING #-} HasElmClient (Post (ct ': cts) ()) where
  elmClientWithRoute Proxy request =
    [(setHttpMethod "POST"
      . addFnSignature "()"
      . setDecoder "(succeed ())") request]
