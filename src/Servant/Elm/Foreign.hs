{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Elm.Foreign where

import           Data.Proxy      (Proxy (Proxy))
import           Elm             (ElmDatatype, ElmType, toElmType)
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor)


data LangElm

instance (ElmType a) => HasForeignType LangElm ElmDatatype a where
  typeFor _ _ _ =
    toElmType (Proxy :: Proxy a)

getEndpoints
  :: ( HasForeign LangElm ElmDatatype api
     , GenerateList ElmDatatype (Foreign ElmDatatype api))
  => Proxy api
  -> [Req ElmDatatype]
getEndpoints =
  listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmDatatype)
