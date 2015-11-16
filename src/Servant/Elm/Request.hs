{-# LANGUAGE TypeOperators #-}

module Servant.Elm.Request where

import           Servant.API     ((:<|>) ((:<|>)))
import           Servant.Foreign (QueryArg, Segment)


-- TODO: rename Result -> Request


data Result = Result
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


defResult :: Result
defResult = Result
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


addTypeDef :: Maybe String -> Result -> Result
addTypeDef (Just elmType) result = result { typeDefs = elmType : typeDefs result }
addTypeDef _ result = result


addFnName :: String -> Result -> Result
addFnName name result = result { fnName = name : fnName result }


addUrlSegment :: Segment -> Result -> Result
addUrlSegment segment result = result { urlSegments = segment : urlSegments result }


addUrlQueryStr :: QueryArg -> Result -> Result
addUrlQueryStr arg result = result { urlQueryStr = arg : urlQueryStr result }


addFnSignature :: String -> Result -> Result
addFnSignature name result = result { fnSignature = name : fnSignature result }


setDecoder :: String -> Result -> Result
setDecoder dec result = result { decoder = dec  }


addDecoderDef :: Maybe String -> Result -> Result
addDecoderDef (Just decoderDef) result = result { decoderDefs = decoderDef : decoderDefs result }
addDecoderDef _ result = result


addArgName :: String -> Result -> Result
addArgName name result = result { argNames = name : argNames result }


setHttpMethod :: String -> Result -> Result
setHttpMethod method result = result { httpMethod = method }


class MakeResultsList results where
  makeResultsList :: results -> [Result]


instance MakeResultsList Result where
  makeResultsList r = [r]


instance (MakeResultsList start, MakeResultsList rest) => MakeResultsList (start :<|> rest) where
  makeResultsList (start :<|> rest) = makeResultsList start ++ makeResultsList rest
