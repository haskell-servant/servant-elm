{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Elm.Foreign where

import           Data.Proxy      (Proxy (Proxy))
import           Elm             (ToElmType, toElmDecoderWithSources,
                                  toElmEncoderWithSources, toElmTypeWithSources)
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor)


data LangElm

data GeneratedElm = GeneratedElm
  { elmType           :: String
  , elmTypeSources    :: [String]
  , elmDecoder        :: String
  , elmDecoderSources :: [String]
  , elmEncoder        :: String
  , elmEncoderSources :: [String]
  } deriving (Show)

instance (ToElmType a) => HasForeignType LangElm GeneratedElm a where
  typeFor _ _ _ =
    let
      proxy =
        Proxy :: Proxy a
      (eType, eTypeSources) =
        toElmTypeWithSources proxy
      (eDecoder, eDecoderSources) =
        toElmDecoderWithSources proxy
      (eEncoder, eEncoderSources) =
        toElmEncoderWithSources proxy
    in
      GeneratedElm
        { elmType = eType
        , elmTypeSources = eTypeSources
        , elmDecoder = eDecoder
        , elmDecoderSources = eDecoderSources
        , elmEncoder = eEncoder
        , elmEncoderSources = eEncoderSources
        }

getEndpoints
  :: ( HasForeign LangElm GeneratedElm api
     , GenerateList GeneratedElm (Foreign GeneratedElm api))
  => Proxy api
  -> [Req GeneratedElm]
getEndpoints api = listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy GeneratedElm) api
