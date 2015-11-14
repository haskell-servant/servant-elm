{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Elm.Client where

import           Data.Proxy          (Proxy (Proxy))
import           Elm                 (ElmType (..), ToElmType,
                                      toElmDecoderSource, toElmType,
                                      toElmTypeSource)
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Servant.API         ((:<|>) ((:<|>)), (:>), Get, Post)

import           Servant.Elm.Request (Result (..), addDecoder, addElmType,
                                      addFnName, addFnSignature, defResult,
                                      setHttpMethod)


toElmTypeName :: (ToElmType a) => a -> String
toElmTypeName = render . toElmType
  where render :: ElmType -> String
        render (DataType d _) = d


elmClient :: (HasElmClient layout)
          => Proxy layout -> ElmClient layout
elmClient p = elmClientWithRoute p defResult


class HasElmClient layout where
  type ElmClient layout :: *
  elmClientWithRoute :: Proxy layout -> Result -> ElmClient layout


instance (HasElmClient a, HasElmClient b) => HasElmClient (a :<|> b) where
  type ElmClient (a :<|> b) = ElmClient a :<|> ElmClient b
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy a) result :<|>
    elmClientWithRoute (Proxy :: Proxy b) result


-- instance (KnownSymbol capture, ToElmType a, HasElmClient sublayout)
--       => HasElmClient (Capture capture a :> sublayout) where
--   type ElmClient (Capture capture a :> sublayout) = a -> ElmClient sublayout
--   elmClientWithRoute Proxy result val =
--     elmClientWithRoute (Proxy :: Proxy sublayout)
--                     (addElmType (toElmTypeSource (Proxy :: Proxy a)) result)


-- Get '[cts] ResultType
instance {-# OVERLAPPABLE #-} (ToElmType apiResult) => HasElmClient (Get (ct ': cts) apiResult) where
  type ElmClient (Get (ct ': cts) apiResult) = Result
  elmClientWithRoute Proxy =
    let
      apiResultProxy = Proxy :: Proxy apiResult
    in
      setHttpMethod "GET"
      . addFnSignature (toElmTypeName apiResultProxy)
      . addElmType (toElmTypeSource apiResultProxy)
      . addDecoder (toElmDecoderSource apiResultProxy)


-- Get '[cts] ()
instance {-# OVERLAPPING #-} HasElmClient (Get (ct ': cts) ()) where
  type ElmClient (Get (ct ': cts) ()) = Result
  elmClientWithRoute Proxy =
    setHttpMethod "GET"
    . addFnSignature "()"


-- Post '[cts] ResultType
instance {-# OVERLAPPABLE #-} (ToElmType apiResult) => HasElmClient (Post (ct ': cts) apiResult) where
  type ElmClient (Post (ct ': cts) apiResult) = Result
  elmClientWithRoute Proxy =
    let
      apiResultProxy = Proxy :: Proxy apiResult
    in
      setHttpMethod "POST"
      . addFnSignature (toElmTypeName apiResultProxy)
      . addElmType (toElmTypeSource apiResultProxy)
      . addDecoder (toElmDecoderSource apiResultProxy)


-- Get '[cts] ()
instance {-# OVERLAPPING #-} HasElmClient (Post (ct ': cts) ()) where
  type ElmClient (Post (ct ': cts) ()) = Result
  elmClientWithRoute Proxy =
    setHttpMethod "POST"
    . addFnSignature "()"


-- path :> rest
instance (KnownSymbol path, HasElmClient sublayout) => HasElmClient (path :> sublayout) where
  type ElmClient (path :> sublayout) = ElmClient sublayout
  elmClientWithRoute Proxy result =
    elmClientWithRoute (Proxy :: Proxy sublayout)
                       (addFnName (symbolVal (Proxy :: Proxy  path)) result)
