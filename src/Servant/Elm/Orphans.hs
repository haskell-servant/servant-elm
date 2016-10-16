{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Servant.Elm.Orphans where

import           Elm          (ElmType, ElmDatatype, toElmType)
import           GHC.Generics (Generic)
import           Servant.API  (NoContent (NoContent))


instance ElmType ElmDatatype where
  toElmType = id


instance ElmType NoContent
