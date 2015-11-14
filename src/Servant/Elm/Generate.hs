{-# LANGUAGE FlexibleContexts #-}

module Servant.Elm.Generate where

import           Data.Char           (toLower)
import           Data.List           (nub)
import           Data.Proxy          (Proxy)
import qualified Data.Text           as T
import           Servant.Elm.Client (HasElmClient, ElmClient, elmClient)
import           Servant.Elm.Request (MakeResultsList, Result (..),
                                      makeResultsList)
import           Servant.Foreign     (camelCase)


generateElmForAPI :: (HasElmClient layout, MakeResultsList (ElmClient layout))
                  => Proxy layout -> [String]
generateElmForAPI = nub . concatMap generateElmForResult . makeResultsList . elmClient


generateElmForResult :: Result -> [String]
generateElmForResult result = types result ++ decoders result ++ [func]
  where func = funcName ++ " : " ++ typeSignature (fnSignature result) ++ "\n"
                  ++ funcName ++ " =\n"
                  ++ "  let request =\n"
                  ++ "        { verb = \"" ++ httpMethod result ++ "\"\n"
                  -- TODO: headers, url, body, content type?
                  ++ "        , headers = [(\"Content-Type\", \"application/json\")]\n"
                  ++ "        , url = \"\"\n"
                  ++ "        , body = Http.empty\n"
                  ++ "        }\n"
                  ++ "  in  Http.fromJson\n"
                  ++ "        " ++ decoderName ++ "\n"
                  ++ "        (Http.send Http.defaultSettings request)"
        funcName = (T.unpack . camelCase . map T.pack . (:) (map toLower (httpMethod result)) . reverse) (fnName result)
        typeSignature [x] = "Task.Task Http.Error " ++ x
        typeSignature (x:xs) = x ++ " -> " ++ typeSignature xs
        typeSignature [] = ""
        decoderName = "decode" ++ head (fnSignature result)
