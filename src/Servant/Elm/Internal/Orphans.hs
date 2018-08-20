{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Servant.Elm.Internal.Orphans where

import           Elm         (ElmDatatype, ElmType, toElmType)
import           Servant.API (NoContent, Headers, getResponse)


instance ElmType ElmDatatype where
  toElmType = id


instance ElmType NoContent

-- TODO: Generate Elm functions that can handle the response headers. PRs
-- welcome!
instance (ElmType a) => ElmType (Headers ls a) where
  toElmType = toElmType . getResponse
