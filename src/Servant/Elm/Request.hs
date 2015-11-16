{-# LANGUAGE TypeOperators #-}

module Servant.Elm.Request where

import           Servant.API     ((:<|>) ((:<|>)))
import           Servant.Foreign (QueryArg, Segment)


data Request = Request
  { typeDefs    :: [String]
  , decoderDefs :: [String]
  , decoder     :: String
  , fnName      :: [String]
  , fnSignature :: [String]
  , urlSegments :: [Segment]
  , urlQueryStr :: [QueryArg]
  , httpMethod  :: String
  , argNames    :: [String]
  } deriving (Show)


defRequest :: Request
defRequest = Request
  { typeDefs = []
  , decoderDefs = []
  , decoder = ""
  , fnName = []
  , fnSignature = []
  , httpMethod = "GET"
  , urlSegments = []
  , urlQueryStr = []
  , argNames = []
  }


addTypeDef :: Maybe String -> Request -> Request
addTypeDef (Just elmType) result = result { typeDefs = elmType : typeDefs result }
addTypeDef _ result = result


addFnName :: String -> Request -> Request
addFnName name result = result { fnName = name : fnName result }


addUrlSegment :: Segment -> Request -> Request
addUrlSegment segment result = result { urlSegments = segment : urlSegments result }


addUrlQueryStr :: QueryArg -> Request -> Request
addUrlQueryStr arg result = result { urlQueryStr = arg : urlQueryStr result }


addFnSignature :: String -> Request -> Request
addFnSignature name result = result { fnSignature = name : fnSignature result }


setDecoder :: String -> Request -> Request
setDecoder dec result = result { decoder = dec  }


addDecoderDef :: Maybe String -> Request -> Request
addDecoderDef (Just decoderDef) result = result { decoderDefs = decoderDef : decoderDefs result }
addDecoderDef _ result = result


addArgName :: String -> Request -> Request
addArgName name result = result { argNames = name : argNames result }


setHttpMethod :: String -> Request -> Request
setHttpMethod method result = result { httpMethod = method }
