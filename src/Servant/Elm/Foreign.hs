{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Elm.Foreign where

import           Data.Proxy      (Proxy (Proxy))
import           Elm             (ElmType, ElmTypeExpr, toElmType)
import           GHC.Generics    (Generic)
import           Servant.API     (NoContent (NoContent))
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor)


data LangElm

instance ElmType ElmTypeExpr where
  toElmType = id

deriving instance Generic NoContent
instance ElmType NoContent

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
