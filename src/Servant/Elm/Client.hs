{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Elm.Client where

import           Data.Proxy          (Proxy (Proxy))
import qualified Data.Text           as T
import           Elm                 (ToElmType, toElmType', toElmDecoder)
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Servant.API         ((:<|>), (:>), Capture, Get, Post,
                                      QueryFlag, QueryParam, QueryParams)
import           Servant.Foreign     (ArgType (..), QueryArg (..), Segment (..),
                                      SegmentType (..))

import           Servant.Elm.Request (Request (..), addArgName, addDecoderDefs,
                                      addFnName, addFnSignature, addTypeDefs,
                                      addUrlQueryStr, addUrlSegment, defRequest,
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
                       ((addTypeDefs typeDefs
                         . addFnSignature typeName
                         . addFnName "by"
                         . addArgName argName
                         . addUrlSegment (Segment (Cap (T.pack argName)))) result)
      where argProxy = Proxy :: Proxy a
            argName = symbolVal (Proxy :: Proxy capture)
            (typeName, typeDefs) = toElmType' argProxy


-- QueryFlag name :> rest
instance (KnownSymbol sym, HasElmClient sublayout)
      => HasElmClient (QueryFlag sym :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addArgName argName
                         . addFnSignature (fst (toElmType' (Proxy :: Proxy Bool)))
                         . addUrlQueryStr (QueryArg (T.pack argName) Flag)) result)
      where argName = symbolVal (Proxy :: Proxy sym)


-- QueryParams name ArgType :> rest
instance (KnownSymbol sym, ToElmType a, HasElmClient sublayout)
      => HasElmClient (QueryParams sym a :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addArgName argName
                         . addTypeDefs typeDefs
                         . addFnSignature typeName
                         . addUrlQueryStr (QueryArg (T.pack argName) List)) result)
      where argName = symbolVal (Proxy :: Proxy sym)
            (typeName, typeDefs) = toElmType' (Proxy :: Proxy [a])


-- QueryParam name ArgType :> rest
instance (KnownSymbol sym, ToElmType a, HasElmClient sublayout)
      => HasElmClient (QueryParam sym a :> sublayout) where
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       ((addArgName argName
                         . addTypeDefs typeDefs
                         . addFnSignature typeName
                         . addUrlQueryStr (QueryArg (T.pack argName) Normal)) result)
      where argName = symbolVal (Proxy :: Proxy sym)
            (typeName, typeDefs) = toElmType' (Proxy :: Proxy a)


-- Get '[cts] RequestType :> rest
instance {-# OVERLAPPABLE #-} (ToElmType apiRequest) => HasElmClient (Get (ct ': cts) apiRequest) where
  elmClientWithRoute Proxy request =
    [completeRequestWithType (Proxy :: Proxy apiRequest) "GET" request]


-- Get '[cts] () :> rest
instance {-# OVERLAPPING #-} HasElmClient (Get (ct ': cts) ()) where
  elmClientWithRoute Proxy request =
    [completeRequest "GET" request]


-- Post '[cts] RequestType :> rest
instance {-# OVERLAPPABLE #-} (ToElmType apiRequest) => HasElmClient (Post (ct ': cts) apiRequest) where
  elmClientWithRoute Proxy request =
    [completeRequestWithType (Proxy :: Proxy apiRequest) "POST" request]


-- Post '[cts] () :> rest
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
    (dec, decDefs) = toElmDecoder proxy
    (typeName, typeDefs) = toElmType' proxy


completeRequest :: String -> Request -> Request
completeRequest method =
  setHttpMethod method
  . addFnSignature "()"
  . setDecoder "(succeed ())"
