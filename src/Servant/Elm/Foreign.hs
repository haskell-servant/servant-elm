{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Elm.Foreign where

import           Data.Proxy      (Proxy (Proxy))
import           Elm             (ElmType, toElmDecoderSourceDefs,
                                  toElmEncoderSourceDefs, toElmTypeSourceDefs)
import           GHC.Generics    (Generic)
import           Servant.API     (NoContent (NoContent))
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
  , elmDecoder        :: Maybe String
    -- Elm definitions required to use the decoder.
    -- E.g. "decodeBlogPost = ...; decodeComment = ..."
  , elmDecoderSources :: [String]
    -- The name of the JSON encoder for the type.
    -- E.g. "encodeBlogPost"
  , elmEncoder        :: Maybe String
    -- Elm definitions required to use the encoder.
    -- E.g. "encodeBlogPost = ...; encodeComment = ..."
  , elmEncoderSources :: [String]
  } deriving (Show)

instance {-# Overlappable #-} (ElmType a) => HasForeignType LangElm GeneratedElm a where
  typeFor _ _ _ =
    let
      proxy =
        Proxy :: Proxy a
      (eType, eTypeSources) =
        toElmTypeSourceDefs proxy
      (eDecoder, eDecoderSources) =
        toElmDecoderSourceDefs proxy
      (eEncoder, eEncoderSources) =
        toElmEncoderSourceDefs proxy
    in
      GeneratedElm
        { elmType = eType
        , elmTypeSources = eTypeSources
        , elmDecoder = Just eDecoder
        , elmDecoderSources = eDecoderSources
        , elmEncoder = Just eEncoder
        , elmEncoderSources = eEncoderSources
        }


deriving instance Generic NoContent
instance ElmType NoContent
instance {-# Overlapping #-} HasForeignType LangElm GeneratedElm NoContent where
  typeFor _ _ = typeForNoJSON


instance {-# Overlapping #-} HasForeignType LangElm GeneratedElm () where
  typeFor _ _ = typeForNoJSON


typeForNoJSON :: ElmType a => a -> GeneratedElm
typeForNoJSON x =
  let
    (eType, eTypeSources) =
      toElmTypeSourceDefs x
  in
    GeneratedElm
      { elmType = eType
      , elmTypeSources = eTypeSources
      , elmDecoder = Nothing
      , elmDecoderSources = []
      , elmEncoder = Nothing
      , elmEncoderSources = []
      }


getEndpoints
  :: ( HasForeign LangElm GeneratedElm api
     , GenerateList GeneratedElm (Foreign GeneratedElm api))
  => Proxy api
  -> [Req GeneratedElm]
getEndpoints =
  listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy GeneratedElm)
