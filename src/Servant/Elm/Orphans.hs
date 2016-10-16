{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Servant.Elm.Orphans where

import           Elm          (ElmType, ElmDatatype, toElmType)
import           Servant.API  (NoContent)


instance ElmType ElmDatatype where
  toElmType = id


instance ElmType NoContent
