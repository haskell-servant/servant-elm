module Servant.Elm.Internal.Options where

import qualified Data.Aeson as A
import qualified Elm.Derive as E

defaultOptions :: A.Options
defaultOptions = E.defaultOptions { A.unwrapUnaryRecords = False}

