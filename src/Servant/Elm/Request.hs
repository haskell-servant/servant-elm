{-# LANGUAGE TypeOperators #-}

module Servant.Elm.Request where

import           Servant.Foreign (QueryArg, Segment)


data Request = Request
  { typeDefs    :: [String]
  , decoderDefs :: [String]
  , encoderDefs :: [String]
  , decoder     :: String
  , fnName      :: [String]
  , fnSignature :: [String]
  , urlSegments :: [Segment]
  , urlQueryStr :: [QueryArg]
  , httpMethod  :: String
  , argNames    :: [String]
  , bodyEncoder :: Maybe String
  } deriving (Show)


defRequest :: Request
defRequest = Request
  { typeDefs = []
  , decoderDefs = []
  , encoderDefs = []
  , decoder = ""
  , fnName = []
  , fnSignature = []
  , httpMethod = "GET"
  , urlSegments = []
  , urlQueryStr = []
  , argNames = []
  , bodyEncoder = Nothing
  }


addTypeDefs :: [String] -> Request -> Request
addTypeDefs elmTypes result = result { typeDefs = typeDefs result ++ elmTypes }


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


addDecoderDefs :: [String] -> Request -> Request
addDecoderDefs defs result = result { decoderDefs = decoderDefs result ++ defs }


addArgName :: String -> Request -> Request
addArgName name result = result { argNames = name : argNames result }


setHttpMethod :: String -> Request -> Request
setHttpMethod method result = result { httpMethod = method }


setBodyEncoder :: String -> Request -> Request
setBodyEncoder encoder request = request { bodyEncoder = Just encoder }


addEncoderDefs :: [String] -> Request -> Request
addEncoderDefs defs result = result { encoderDefs = encoderDefs result ++ defs }
