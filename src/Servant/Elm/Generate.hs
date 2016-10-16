{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Servant.Elm.Generate where

import           Control.Lens        (view, (^.))
import           Data.List           (nub)
import           Data.Maybe          (catMaybes)
import           Data.Monoid         ((<>))
import           Data.Proxy          (Proxy)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Elm                 (ElmDatatype)
import qualified Elm
import           Formatting          (Format, now, sformat, stext, (%))
import           Servant.API         (NoContent (..))
import           Servant.Elm.Foreign (LangElm, getEndpoints)
import           Servant.Elm.Orphans ()
import qualified Servant.Foreign     as F


cr :: Format r r
cr = now "\n"


{-|
Options to configure how code is generated.
-}
data ElmOptions = ElmOptions
  { {- | The protocol, host and any path prefix to be used as the base for all
    requests.

    Example: @"https://mydomain.com/api/v1"@
    -}
    urlPrefix             :: T.Text
  , elmExportOptions      :: Elm.Options
    -- ^ Options to pass to elm-export
  , emptyResponseElmTypes :: [ElmDatatype]
    -- ^ Types that represent an empty Http response.
  , stringElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a String.
  }


{-|
Default options for generating Elm code.

The default options are:

> { urlPrefix =
>     ""
> , elmExportOptions =
>     Elm.defaultOptions
> , emptyResponseElmTypes =
>     [ toElmType NoContent ]
> , stringElmTypes =
>     [ toElmType "" ]
> }
-}
defElmOptions :: ElmOptions
defElmOptions = ElmOptions
  { urlPrefix = ""
  , elmExportOptions = Elm.defaultOptions
  , emptyResponseElmTypes =
      [ Elm.toElmType NoContent
      ]
  , stringElmTypes =
      [ Elm.toElmType ("" :: String)
      ]
  }


{-|
Default imports required by generated Elm code.

You probably want to include this at the top of your generated Elm module.

The default required imports are:

> import Json.Decode exposing (..)
> import Json.Decode.Pipeline exposing (..)
> import Json.Encode
> import Http
> import String
> import Task
-}
defElmImports :: Text
defElmImports =
  T.unlines
    [ "import Json.Decode exposing (..)"
    , "import Json.Decode.Pipeline exposing (..)"
    , "import Json.Encode"
    , "import Http"
    , "import String"
    , "import Task"
    ]


{-|
Generate Elm code for the API with default options.

Returns a list of Elm functions to query your Servant API from Elm.

You could spit these out to a file and call them from your Elm code, but you
would be better off creating a 'Spec' with the result and using 'specsToDir',
which handles the module name for you.
-}
generateElmForAPI
  :: ( F.HasForeign LangElm ElmDatatype api
     , F.GenerateList ElmDatatype (F.Foreign ElmDatatype api))
  => Proxy api
  -> [Text]
generateElmForAPI =
  generateElmForAPIWith defElmOptions


{-|
Generate Elm code for the API with custom options.
-}
generateElmForAPIWith
  :: ( F.HasForeign LangElm ElmDatatype api
     , F.GenerateList ElmDatatype (F.Foreign ElmDatatype api))
  => ElmOptions
  -> Proxy api
  -> [Text]
generateElmForAPIWith opts =
  nub . concatMap (generateElmForRequest opts) . getEndpoints

{-|
Generate an Elm function for one endpoint.

This function returns a list because the query function may require some
supporting definitions.
-}
generateElmForRequest :: ElmOptions -> F.Req ElmDatatype -> [Text]
generateElmForRequest opts request =
  supportingFunctions
  ++ [funcDef]
  where
    funcDef =
      sformat
        (stext % " : " % stext % cr %
         stext % " =" % cr %
         "  let" % cr %
         -- stext % cr %
         stext %
         stext % cr %
         "  in" % cr %
         stext
        )
        fnName
        typeSignature
        fnNameArgs
        (case letParams of
           Just x -> x <> "\n"
           Nothing -> ""
        )
        letRequest
        httpRequest

    fnName =
      (F.camelCase (request ^. F.reqFuncName))

    typeSignature =
      mkTypeSignature opts request

    fnNameArgs =
      T.unwords (fnName : mkArgsList request)

    letParams =
      mkLetParams "    " opts request

    letRequest =
      mkLetRequest "    " opts request

    (httpRequest, supportingFunctions) =
      mkHttpRequest "    " opts request


mkTypeSignature
  :: ElmOptions
  -> F.Req ElmDatatype
  ->  Text
mkTypeSignature opts request =
    T.intercalate " -> "
    ( urlCaptureTypes
    ++ queryTypes
    ++ catMaybes [bodyType, returnType])
  where
    getElmName :: ElmDatatype -> Text
    getElmName eType =
      Elm.toElmTypeRefWith (elmExportOptions opts) eType

    urlCaptureTypes :: [Text]
    urlCaptureTypes =
        [ getElmName (F.captureArg cap ^. F.argType)
        | cap <- request ^. F.reqUrl . F.path
        , F.isCapture cap
        ]

    queryTypes :: [Text]
    queryTypes =
      [ case arg ^. F.queryArgType of
          F.Normal ->
            getElmName (Elm.ElmPrimitive . Elm.EMaybe $ arg ^. F.queryArgName . F.argType)
          _ ->
            getElmName (arg ^. F.queryArgName . F.argType)
      | arg <- request ^. F.reqUrl . F.queryStr
      ]

    bodyType :: Maybe Text
    bodyType =
        getElmName <$> (request ^. F.reqBody)

    returnType :: Maybe Text
    returnType =
      (sformat ("Task.Task Http.Error (" % stext % ")")) . getElmName <$> (request ^. F.reqReturnType)


mkArgsList
  :: F.Req ElmDatatype
  -> [Text]
mkArgsList request =
  -- URL Captures
  [ F.unPathSegment $ F.captureArg segment ^. F.argName
  | segment <- request ^. F.reqUrl . F.path
  , F.isCapture segment
  ]
  ++
  -- Query params
  [ F.unPathSegment $ arg ^. F.queryArgName . F.argName
  | arg <- request ^. F.reqUrl . F.queryStr
  ]
  ++
  -- Request body
  catMaybes
    [ fmap (const "body") (request ^. F.reqBody)
    ]


mkUrl
  :: ElmOptions
  -> [F.Segment ElmDatatype]
  -> Text
mkUrl opts segments =
  (T.intercalate newLine . catMaybes)
    [ if T.null (urlPrefix opts) then
        Nothing
      else
        Just $ "\"" <> urlPrefix opts <> "\""
    , if null segments then
        Nothing
      else
        Just $ "\"/\" ++ "
               <> T.intercalate (newLine <> "\"/\" ++ ")
                                (map segmentToText segments)
    ]
  where
    newLine =
      "\n          ++ "

    segmentToText :: F.Segment ElmDatatype -> Text
    segmentToText s =
      case F.unSegment s of
        F.Static path ->
          "\"" <> F.unPathSegment path <> "\""
        F.Cap arg ->
          let
            -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
            toStringSrc =
              if isElmStringType opts (arg ^. F.argType) then
                ""
              else
                " |> toString"
          in
            "(" <> (F.unPathSegment (arg ^. F.argName)) <> toStringSrc <> " |> Http.uriEncode)"


mkLetParams
  :: Text
  -> ElmOptions
  -> F.Req ElmDatatype
  -> Maybe Text
mkLetParams indent opts request =
  if null (request ^. F.reqUrl . F.queryStr) then
    Nothing
  else
    Just $ T.intercalate "\n" $ map (indent <>)
        [ "params ="
        , "  List.filter (not << String.isEmpty)"
        , "    [ " <> T.intercalate ("\n" <> indent <> "    , ") params
        , "    ]"
        ]
  where
    params :: [Text]
    params = map paramToStr (request ^. F.reqUrl . F.queryStr)

    paramToStr :: F.QueryArg ElmDatatype -> Text
    paramToStr qarg =
      case qarg ^. F.queryArgType of
        F.Normal ->
          let
            -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
            toStringSrc =
              if isElmStringType opts (qarg ^. F.queryArgName . F.argType) then
                ""
              else
                "toString >> "
          in
            T.intercalate newLine
              [ name
              , "  |> Maybe.map (" <> toStringSrc <> "Http.uriEncode >> (++) \"" <> name <> "=\")"
              , "  |> Maybe.withDefault \"\""
              ]

        F.Flag ->
          T.intercalate newLine
            ["if " <> name <> " then"
            , "  \"" <> name <> "=\""
            , "else"
            , "  \"\""
            ]

        F.List ->
          T.intercalate newLine
            [ name
            , "  |> List.map (\\val -> \"" <> name <> "[]=\" ++ (val |> toString |> Http.uriEncode))"
            , "  |> String.join \"&\""
            ]
      where
        name =
          F.unPathSegment . view (F.queryArgName . F.argName) $ qarg
        newLine = "\n          "


mkLetRequest
  :: Text
  -> ElmOptions
  -> F.Req ElmDatatype
  -> Text
mkLetRequest indent opts request =
  T.intercalate "\n" $ map (indent <>)
      ([ "request ="
       , "  { verb ="
       , "      \"" <> method <> "\""
       , "  , headers ="
       , "      [(\"Content-Type\", \"application/json\")]"
       , "  , url ="
       , "      " <> url
       ]
       ++ mkQueryParams "      " request
       ++ [ "  , body ="
       , "      " <> body
       , "  }"
       ]
      )
  where
    method =
       T.decodeUtf8 (request ^. F.reqMethod)

    url =
      mkUrl opts (request ^. F.reqUrl . F.path)

    body =
      case request ^. F.reqBody of
        Nothing ->
          "Http.empty"

        Just elmTypeExpr ->
          let
            encoderName =
              Elm.toElmEncoderRefWith (elmExportOptions opts) elmTypeExpr
          in
            sformat ("Http.string (Json.Encode.encode 0 (" % stext % " body))") encoderName


mkQueryParams
  :: Text
  -> F.Req ElmDatatype
  -> [Text]
mkQueryParams indent request =
  if null (request ^. F.reqUrl . F.queryStr) then
    []
  else
    map (indent <>)
    [ "++ if List.isEmpty params then"
    , "     \"\""
    , "   else"
    , "     \"?\" ++ String.join \"&\" params"
    ]


{-| If the return type has a decoder, construct the request using Http.fromJson.
Otherwise, construct an HTTP request that expects an empty response.
-}
mkHttpRequest
  :: Text
  -> ElmOptions
  -> F.Req ElmDatatype
  -> (Text, [Text])
mkHttpRequest indent opts request =
  ( T.intercalate "\n" $ map (indent <>) elmLines
  , supportingFunctions
  )
  where
    (elmLines, supportingFunctions) =
      case request ^. F.reqReturnType of
        Just elmTypeExpr | isEmptyType opts elmTypeExpr ->
            (emptyResponseRequest (Elm.toElmTypeRefWith (elmExportOptions opts) elmTypeExpr)
            , [ emptyResponseHandlerSrc
              , handleResponseSrc
              , promoteErrorSrc
              ]
            )

        Just elmTypeExpr ->
          ( jsonRequest (Elm.toElmDecoderRefWith (elmExportOptions opts) elmTypeExpr)
          , []
          )

        Nothing ->
          error "mkHttpRequest: no reqReturnType?"

    jsonRequest decoder =
      [ "Http.fromJson"
      , "  " <> decoder
      , "  (Http.send Http.defaultSettings request)"
      ]

    emptyResponseRequest elmType =
      [ "Task.mapError promoteError"
      , "  (Http.send Http.defaultSettings request)"
      , "    `Task.andThen`"
      , "      handleResponse (emptyResponseHandler " <> elmType <> ")"
      ]

    emptyResponseHandlerSrc =
      T.intercalate "\n"
        [ "emptyResponseHandler : a -> String -> Task.Task Http.Error a"
        , "emptyResponseHandler x str ="
        , "  if String.isEmpty str then"
        , "    Task.succeed x"
        , "  else"
        , "    Task.fail (Http.UnexpectedPayload str)"
        ]

    handleResponseSrc =
      T.intercalate "\n"
        [ "handleResponse : (String -> Task.Task Http.Error a) -> Http.Response -> Task.Task Http.Error a"
        , "handleResponse handle response ="
        , "  if 200 <= response.status && response.status < 300 then"
        , "    case response.value of"
        , "      Http.Text str ->"
        , "        handle str"
        , "      _ ->"
        , "        Task.fail (Http.UnexpectedPayload \"Response body is a blob, expecting a string.\")"
        , "  else"
        , "    Task.fail (Http.BadResponse response.status response.statusText)"
        ]

    promoteErrorSrc =
      T.intercalate "\n"
        [ "promoteError : Http.RawError -> Http.Error"
        , "promoteError rawError ="
        , "  case rawError of"
        , "    Http.RawTimeout -> Http.Timeout"
        , "    Http.RawNetworkError -> Http.NetworkError"
        ]


{- | Determines whether we construct an Elm function that expects an empty
response body.
-}
isEmptyType :: ElmOptions -> ElmDatatype -> Bool
isEmptyType opts elmTypeExpr =
  elmTypeExpr `elem` emptyResponseElmTypes opts


{- | Determines whether we call `toString` on URL captures and query params of
this type in Elm.
-}
isElmStringType :: ElmOptions -> ElmDatatype -> Bool
isElmStringType opts elmTypeExpr =
  elmTypeExpr `elem` stringElmTypes opts
