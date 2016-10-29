{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Servant.Elm.Internal.Orphans where

import           Elm         (ElmDatatype, ElmType, toElmType)
import           Servant.API (NoContent)


instance ElmType ElmDatatype where
  toElmType = id


instance ElmType NoContent
