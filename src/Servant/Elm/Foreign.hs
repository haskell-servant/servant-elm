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
  { -- The Elm type of the thing we're looking at.
    -- E.g. "BlogPost"
    elmType           :: String
    -- The Elm definitions required to use the type.
    -- E.g. "type BlogPost = ...; type Comment = ..."
  , elmTypeSources    :: [String]
    -- The name of the JSON decoder for the type.
    -- E.g. "decodeBlogPost"
  , elmDecoder        :: String
    -- Elm definitions required to use the decoder.
    -- E.g. "decodeBlogPost = ...; decodeComment = ..."
  , elmDecoderSources :: [String]
    -- The name of the JSON encoder for the type.
    -- E.g. "encodeBlogPost"
  , elmEncoder        :: String
    -- Elm definitions required to use the encoder.
    -- E.g. "encodeBlogPost = ...; encodeComment = ..."
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
getEndpoints =
  listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy GeneratedElm)
