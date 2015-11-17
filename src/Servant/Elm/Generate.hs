{-# LANGUAGE FlexibleContexts #-}

module Servant.Elm.Generate where

import           Data.Char           (toLower)
import           Data.List           (nub)
import           Data.Proxy          (Proxy)
import qualified Data.Text           as T
import           Servant.Elm.Client  (HasElmClient, elmClient)
import           Servant.Elm.Request (Request (..))
import           Servant.Foreign     (ArgType (..), QueryArg (..), Segment (..),
                                      SegmentType (..), camelCase)


data ElmOptions = ElmOptions
  { urlPrefix :: String }


defElmOptions :: ElmOptions
defElmOptions = ElmOptions
  { urlPrefix = "" }


defElmImports :: String
defElmImports =
  unlines
    [ "import Json.Decode exposing (..)"
    , "import Json.Decode.Extra exposing (apply)"
    , "import Json.Encode as JS"
    , "import Http"
    , "import String"
    , "import Task"
    ]


generateElmForAPI :: (HasElmClient layout)
                  => Proxy layout -> [String]
generateElmForAPI = generateElmForAPIWith defElmOptions


generateElmForAPIWith :: (HasElmClient layout)
                      => ElmOptions -> Proxy layout -> [String]
generateElmForAPIWith opts = nub . concatMap (generateElmForRequest opts) . elmClient


-- TODO: headers, query args, body, content type?, encoders?
generateElmForRequest :: ElmOptions -> Request -> [String]
generateElmForRequest opts result = typeDefs result ++ decoderDefs result ++ encoderDefs result ++ [func]
  where func = funcName ++ " : " ++ (typeSignature . reverse . fnSignature) result ++ "\n"
                  ++ funcNameArgs ++ " =\n"
                  ++ "  let request =\n"
                  ++ "        { verb = \"" ++ httpMethod result ++ "\"\n"
                  ++ "        , headers = [(\"Content-Type\", \"application/json\")]\n"
                  ++ "        , url = " ++ url ++ "\n"
                  ++ "        , body = " ++ body ++ "\n"
                  ++ "        }\n"
                  ++ "  in  Http.fromJson\n"
                  ++ "        (" ++ decoder result ++ ")\n"
                  ++ "        (Http.send Http.defaultSettings request)"
        funcName = (T.unpack . camelCase . map T.pack . (:) (map toLower (httpMethod result)) . reverse) (fnName result)
        typeSignature [x] = "Task.Task Http.Error (" ++ x ++ ")"
        typeSignature (x:xs) = x ++ " -> " ++ typeSignature xs
        typeSignature [] = ""
        funcNameArgs = unwords (funcName : args)
        args = reverse (argNames result)
        url = if url' == "\"" then "\"/\"" else url'
        url' = "\""
           ++ urlPrefix opts
           ++ urlArgs
           ++ queryArgs

        urlArgs = (elmSegments . reverse . urlSegments) result

        queryParams = reverse (urlQueryStr result)
        queryArgs = if null queryParams
                      then ""
                      else " ++ \"?" ++ elmParams queryParams
        body = case bodyEncoder result of
                 Just encoder -> "(Http.string (JS.encode 0 (" ++ encoder ++ " body)))"
                 Nothing -> "Http.empty"


elmSegments :: [Segment] -> String
elmSegments []  = "\""
elmSegments [x] = "/" ++ segmentToStr x False
elmSegments (x:xs) = "/" ++ segmentToStr x True ++ elmSegments xs

segmentToStr :: Segment -> Bool -> String
segmentToStr (Segment st) notTheEnd =
  segmentTypeToStr st ++ if notTheEnd then "" else "\""

segmentTypeToStr :: SegmentType -> String
segmentTypeToStr (Static s) = T.unpack s
segmentTypeToStr (Cap s)    = "\" ++ (" ++ T.unpack s ++ " |> toString |> Http.uriEncode) ++ \""

elmGParams :: String -> [QueryArg] -> String
elmGParams _ []     = ""
elmGParams _ [x]    = paramToStr x False
elmGParams s (x:xs) = paramToStr x True ++ s ++ elmGParams s xs

elmParams :: [QueryArg] -> String
elmParams = elmGParams "&"

paramToStr :: QueryArg -> Bool -> String
paramToStr qarg notTheEnd =
  case _argType qarg of
    Normal -> name
           ++ "=\" ++ (" ++ name ++ " |> toString |> Http.uriEncode)"
           ++ if notTheEnd then " ++ \"" else ""
    Flag   -> "\" ++ "
           ++ "if " ++ name
           ++ " then \"" ++ name ++ "=\""
           ++ " else \"\""
           ++ if notTheEnd then " ++ \"" else ""
    List   -> "\" ++ "
           ++ "String.join \"&\" "
           ++ "(List.map "
           ++ "(\\val -> \""
           ++ name
           ++ "[]=\" ++ (val |> toString |> Http.uriEncode)) "
           ++ name ++ ")"
           ++ if notTheEnd then " ++ \"" else ""
  where name = T.unpack (_argName qarg)
