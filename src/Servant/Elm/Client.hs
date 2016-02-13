{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Elm.Client where

import           Data.Proxy          (Proxy (Proxy))
import qualified Data.Text           as T
import           Elm                 (ToElmType, toElmDecoderWithSources,
                                      toElmEncoderWithSources,
                                      toElmTypeWithSources)
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Servant.API         ((:<|>), (:>), Capture, Get, Post,
                                      QueryFlag, QueryParam, QueryParams,
                                      ReqBody)
import           Servant.Foreign     (ArgType (..), QueryArg (..), Segment (..),
                                      SegmentType (..))

import           Servant.Elm.Request (Request (..), addArgName, addDecoderDefs,
                                      addEncoderDefs, addFnName, addFnSignature,
                                      addTypeDefs, addUrlQueryStr,
                                      addUrlSegment, defRequest, setBodyEncoder,
                                      setDecoder, setHttpMethod)


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
                       ((addTypeDefs tDefs
                         . addFnSignature typeName
                         . addFnName "by"
                         . addArgName argName
                         . addUrlSegment (Segment (Cap (T.pack argName, T.pack typeName)))) result)
      where argProxy = Proxy :: Proxy a
            argName = symbolVal (Proxy :: Proxy capture)
            (typeName, tDefs) = toElmTypeWithSources argProxy


-- QueryFlag name :> rest
instance (KnownSymbol sym, HasElmClient sublayout)
      => HasElmClient (QueryFlag sym :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addArgName argName
                         . addFnSignature typeName
                         . addUrlQueryStr (QueryArg (T.pack argName, T.pack typeName) Flag)) result)
      where argName = symbolVal (Proxy :: Proxy sym)
            (typeName, _) = toElmTypeWithSources (Proxy :: Proxy Bool)


-- QueryParams name ArgType :> rest
instance (KnownSymbol sym, ToElmType a, HasElmClient sublayout)
      => HasElmClient (QueryParams sym a :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addArgName argName
                         . addTypeDefs tDefs
                         . addFnSignature typeName
                         . addUrlQueryStr (QueryArg (T.pack argName, T.pack typeName) List)) result)
      where argName = symbolVal (Proxy :: Proxy sym)
            (typeName, tDefs) = toElmTypeWithSources (Proxy :: Proxy [a])


-- QueryParam name ArgType :> rest
instance (KnownSymbol sym, ToElmType a, HasElmClient sublayout)
      => HasElmClient (QueryParam sym a :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addArgName argName
                         . addTypeDefs tDefs
                         . addFnSignature typeName
                         . addUrlQueryStr (QueryArg (T.pack argName, T.pack typeName) Normal)) result)
      where argName = symbolVal (Proxy :: Proxy sym)
            (typeName, tDefs) = toElmTypeWithSources (Proxy :: Proxy (Maybe a))


-- ReqBody '[cts] BodyType :> rest
instance (ToElmType body, HasElmClient sublayout)
      => HasElmClient (ReqBody (ct ': cts) body :> sublayout) where
  elmClientWithRoute Proxy request =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addArgName "body"
                         . addTypeDefs typeDefs
                         . addFnSignature typeName
                         . setBodyEncoder bodyEncoder
                         . addEncoderDefs encoderDefs) request)
      where (typeName, typeDefs) = toElmTypeWithSources (Proxy :: Proxy body)
            (bodyEncoder, encoderDefs) = toElmEncoderWithSources (Proxy :: Proxy body)


-- Get '[cts] RequestType
instance {-# OVERLAPPABLE #-} (ToElmType apiRequest) => HasElmClient (Get (ct ': cts) apiRequest) where
  elmClientWithRoute Proxy request =
    [completeRequestWithType (Proxy :: Proxy apiRequest) "GET" request]


-- Get '[cts] ()
instance {-# OVERLAPPING #-} HasElmClient (Get (ct ': cts) ()) where
  elmClientWithRoute Proxy request =
    [completeRequest "GET" request]


-- Post '[cts] RequestType
instance {-# OVERLAPPABLE #-} (ToElmType apiRequest) => HasElmClient (Post (ct ': cts) apiRequest) where
  elmClientWithRoute Proxy request =
    [completeRequestWithType (Proxy :: Proxy apiRequest) "POST" request]


-- Post '[cts] ()
instance {-# OVERLAPPING #-} HasElmClient (Post (ct ': cts) ()) where
  elmClientWithRoute Proxy request =
    [completeRequest "POST" request]


completeRequestWithType :: ToElmType a => Proxy a -> String -> Request -> Request
completeRequestWithType proxy method =
  setHttpMethod method
  . addFnSignature typeName
  . addTypeDefs typeDefs
  . addDecoderDefs decDefs
  . setDecoder dec
  where
    (dec, decDefs) = toElmDecoderWithSources proxy
    (typeName, typeDefs) = toElmTypeWithSources proxy


completeRequest :: String -> Request -> Request
completeRequest method =
  setHttpMethod method
  . addFnSignature "()"
  . setDecoder "(succeed ())"
