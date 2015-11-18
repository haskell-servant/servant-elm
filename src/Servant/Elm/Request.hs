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
  { typeDefs    = []
  , decoderDefs = []
  , encoderDefs = []
  , decoder     = ""
  , fnName      = []
  , fnSignature = []
  , httpMethod  = "GET"
  , urlSegments = []
  , urlQueryStr = []
  , argNames    = []
  , bodyEncoder = Nothing
  }


addTypeDefs :: [String] -> Request -> Request
addTypeDefs elmTypes request = request { typeDefs = typeDefs request ++ elmTypes }


addFnName :: String -> Request -> Request
addFnName name request = request { fnName = name : fnName request }


addUrlSegment :: Segment -> Request -> Request
addUrlSegment segment request = request { urlSegments = segment : urlSegments request }


addUrlQueryStr :: QueryArg -> Request -> Request
addUrlQueryStr arg request = request { urlQueryStr = arg : urlQueryStr request }


addFnSignature :: String -> Request -> Request
addFnSignature name request = request { fnSignature = name : fnSignature request }


setDecoder :: String -> Request -> Request
setDecoder dec request = request { decoder = dec  }


addDecoderDefs :: [String] -> Request -> Request
addDecoderDefs defs request = request { decoderDefs = decoderDefs request ++ defs }


addArgName :: String -> Request -> Request
addArgName name request = request { argNames = name : argNames request }


setHttpMethod :: String -> Request -> Request
setHttpMethod method request = request { httpMethod = method }


setBodyEncoder :: String -> Request -> Request
setBodyEncoder encoder request = request { bodyEncoder = Just encoder }


addEncoderDefs :: [String] -> Request -> Request
addEncoderDefs defs request = request { encoderDefs = encoderDefs request ++ defs }
