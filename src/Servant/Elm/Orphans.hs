{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Servant.Elm.Orphans where

import           Elm          (ElmType, ElmTypeExpr, toElmType)
import           GHC.Generics (Generic)
import           Servant.API  (NoContent (NoContent))


instance ElmType ElmTypeExpr where
  toElmType = id


deriving instance Generic NoContent
instance ElmType NoContent
