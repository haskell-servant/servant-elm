{-# LANGUAGE TypeOperators #-}

module Servant.Elm.Request where

import           Servant.API ((:<|>) ((:<|>)))


-- TODO: rename Result -> Request


data Result = Result
  { types       :: [String]
  , decoders    :: [String]
  , fnName      :: [String]
  , fnSignature :: [String]
  , httpMethod  :: String
  } deriving (Show)


defResult :: Result
defResult = Result
  { types = []
  , decoders = []
  , fnName = []
  , fnSignature = []
  , httpMethod = "GET"
  }


addElmType :: String -> Result -> Result
addElmType elmType result = result { types = elmType : types result }


addFnName :: String -> Result -> Result
addFnName name result = result { fnName = name : fnName result }


addFnSignature :: String -> Result -> Result
addFnSignature name result = result { fnSignature = name : fnSignature result }


addDecoder :: String -> Result -> Result
addDecoder decoder result = result { decoders = decoder : decoders result }


setHttpMethod :: String -> Result -> Result
setHttpMethod method result = result { httpMethod = method }


class MakeResultsList results where
  makeResultsList :: results -> [Result]


instance MakeResultsList Result where
  makeResultsList r = [r]


instance (MakeResultsList start, MakeResultsList rest) => MakeResultsList (start :<|> rest) where
  makeResultsList (start :<|> rest) = makeResultsList start ++ makeResultsList rest
