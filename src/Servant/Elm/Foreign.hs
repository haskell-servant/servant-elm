{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Elm.Foreign where

import           Data.Proxy      (Proxy (Proxy))
import           Elm             (ElmType, ElmTypeExpr, toElmType)
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor)


data LangElm

instance (ElmType a) => HasForeignType LangElm ElmTypeExpr a where
  typeFor _ _ _ =
    toElmType (Proxy :: Proxy a)

getEndpoints
  :: ( HasForeign LangElm ElmTypeExpr api
     , GenerateList ElmTypeExpr (Foreign ElmTypeExpr api))
  => Proxy api
  -> [Req ElmTypeExpr]
getEndpoints =
  listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmTypeExpr)
