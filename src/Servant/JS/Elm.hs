{-# LANGUAGE OverloadedStrings #-}

module Servant.JS.Elm
  ( elmJS
  , elmJSWith
  ) where

import           Control.Lens        (view, (^.), (^..))
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Servant.Foreign     (ArgType (Flag, List, Normal), QueryArg,
                                      Segment (Segment),
                                      SegmentType (Static, Cap), argName,
                                      argType, captureArg, funcName,
                                      headerArgName, isCapture, path, queryStr,
                                      reqBody, reqHeaders, reqMethod, reqUrl)
import           Servant.JS.Internal (AjaxReq, CommonGeneratorOptions,
                                      JavaScriptGenerator,
                                      defCommonGeneratorOptions,
                                      functionNameBuilder, moduleName,
                                      requestBody, toValidFunctionName,
                                      urlPrefix)

elmJS :: JavaScriptGenerator
elmJS = elmJSWith defCommonGeneratorOptions

elmJSWith :: CommonGeneratorOptions -> JavaScriptGenerator
elmJSWith opts reqs =
  let funcs = mconcat . map (generateElmJSWith opts) $ reqs
  in
      "module " <> moduleName opts <> " (..) where\n"
      <> "import Http exposing (Error, empty, fromJson, send, defaultSettings, uriEncode, string)\n"
      <> "import Json.Decode\n"
      <> "import String\n"
      <> "import Task exposing (Task)\n"
      <> funcs

{- TODO:
* headers, ...?
-}
generateElmJSWith :: CommonGeneratorOptions -> AjaxReq -> Text
generateElmJSWith opts req = "\n"
  <> fname <> " : Json.Decode.Decoder a " <> argTypesStr <> " -> Task Error a\n"
  <> fname <> " decoder " <> argsStr <> " =\n"
  <> "  let request =\n"
  <> "        { verb = \"" <> method <> "\"\n"
  <> "        , headers =\n"
  <> "            [(\"Content-Type\", \"application/json\")]\n"
  <> "        , url = " <> url <> "\n"
  <> "        , body = " <> dataBody <> "\n"
  <> "        }\n"
  <> "  in\n"
  <> "      fromJson decoder (send defaultSettings request)\n"
  <> "\n"

  where fname = functionNameBuilder opts (req ^. funcName)
        argsStr = T.intercalate " " args
        args = captures
            ++ map (view argName) queryparams
            ++ body
            ++ map (toValidFunctionName . (<>) "header" . headerArgName) hs

        argTypesStr = T.intercalate " " argTypes
        argTypes = map stringType captures
                ++ map queryArgToType queryparams
                ++ map stringType body
                ++ map stringType hs

        stringType _ = "-> String"
        queryArgToType qarg =
          case qarg ^. argType of
            Normal -> "-> Maybe String"
            Flag   -> "-> Bool"
            List   -> "-> List String"

        captures = map captureArg
                 . filter isCapture
                 $ req ^. reqUrl.path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = [requestBody opts | req ^. reqBody]

        dataBody =
          if req ^. reqBody
            then "string body\n"
            else "empty"

        method = req ^. reqMethod

        url = if url' == "\"" then "\"/\"" else url'
        url' = "\""
           <> urlPrefix opts
           <> urlArgs
           <> queryArgs

        urlArgs = elmSegments
                $ req ^.. reqUrl.path.traverse

        queryArgs = if null queryparams
                      then ""
                      else " ++ \"?" <> elmParams queryparams


-- The following taken from Servant.JS.Internal, with single quotes replaced by
-- double quotes, and encodeURIComponent replaced by Http.uriEncode.

elmSegments :: [Segment] -> Text
elmSegments []  = ""
elmSegments [x] = "/" <> segmentToStr x False
elmSegments (x:xs) = "/" <> segmentToStr x True <> elmSegments xs

segmentToStr :: Segment -> Bool -> Text
segmentToStr (Segment st) notTheEnd =
  segmentTypeToStr st <> if notTheEnd then "" else "\""

segmentTypeToStr :: SegmentType -> Text
segmentTypeToStr (Static s) = s
segmentTypeToStr (Cap s)    = "\" ++ (uriEncode " <> s <> ") ++ \""

elmGParams :: Text -> [QueryArg] -> Text
elmGParams _ []     = ""
elmGParams _ [x]    = paramToStr x False
elmGParams s (x:xs) = paramToStr x True <> s <> elmGParams s xs

elmParams :: [QueryArg] -> Text
elmParams = elmGParams "&"

paramToStr :: QueryArg -> Bool -> Text
paramToStr qarg notTheEnd =
  case qarg ^. argType of
    Normal -> name
           <> "=\" ++ (uriEncode " <> name <> ")"
           <> if notTheEnd then " ++ \"" else ""
    Flag   -> "\" ++ "
           <> "if " <> name
           <> " then \"" <> name <> "=\""
           <> " else \"\""
           <> if notTheEnd then " ++ \"" else ""
    List   -> "\" ++ "
           <> "String.join \"&\" "
           <> "(List.map "
           <> "(\\val -> \""
           <> name
           <> "[]=\" ++ (uriEncode val)) "
           <> name <> ")"
           <> if notTheEnd then " ++ \"" else ""
  where name = qarg ^. argName
