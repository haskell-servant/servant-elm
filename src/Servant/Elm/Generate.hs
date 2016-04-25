{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.Elm.Generate where

import           Control.Lens        (view, (^.))
import           Data.Bifunctor      (bimap, first)
import           Data.List           (intercalate, nub)
import           Data.Maybe          (catMaybes)
import           Data.Proxy          (Proxy)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Elm                 (ElmTypeExpr)
import qualified Elm
import Servant.API (NoContent(..))
import           Servant.Elm.Foreign (LangElm, getEndpoints)
import qualified Servant.Foreign     as F


{-|
Options to configure how code is generated.
-}
data ElmOptions = ElmOptions
  { {- | The protocol, host and any path prefix to be used as the base for all
    requests.

    Example: @"https://mydomain.com/api/v1"@
    -}
    urlPrefix        :: String
  , elmExportOptions :: Elm.Options
    -- ^ Options to pass to elm-export
  , emptyResponseElmTypes :: [ElmTypeExpr]
    -- ^ Types that represent an empty Http response.
  , stringElmTypes :: [ElmTypeExpr]
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
>     [ toElmType (), toElmType NoContent ]
> , stringElmTypes =
>     [ toElmType "" ]
> }
-}
defElmOptions :: ElmOptions
defElmOptions = ElmOptions
  { urlPrefix = ""
  , elmExportOptions = Elm.defaultOptions
  , emptyResponseElmTypes =
      [ Elm.toElmType ()
      , Elm.toElmType NoContent
      ]
  , stringElmTypes =
      [ Elm.toElmType ""
      ]
  }


{-|
Default imports required by generated Elm code.

You probably want to include this at the top of your generated Elm module.

The default required imports are:

> import Json.Decode exposing ((:=))
> import Json.Decode.Extra exposing ((|:))
> import Json.Encode
> import Http
> import String
> import Task
-}
defElmImports :: String
defElmImports =
  unlines
    [ "import Json.Decode exposing ((:=))"
    , "import Json.Decode.Extra exposing ((|:))"
    , "import Json.Encode"
    , "import Http"
    , "import String"
    , "import Task"
    ]


{-|
Generate Elm code for the API with default options.

Returns a list of Elm code definitions with everything you need to query your
Servant API from Elm: type definitions, JSON decoders, JSON encoders, and query
functions.

You could spit these out to a file and call them from your Elm code, but you
would be better off creating a 'Spec' with the result and using 'specsToDir',
which handles the module name for you.
-}
generateElmForAPI
  :: ( F.HasForeign LangElm ElmTypeExpr api
     , F.GenerateList ElmTypeExpr (F.Foreign ElmTypeExpr api))
  => Proxy api
  -> [String]
generateElmForAPI =
  generateElmForAPIWith defElmOptions


{-|
Generate Elm code for the API with custom options.
-}
generateElmForAPIWith
  :: ( F.HasForeign LangElm ElmTypeExpr api
     , F.GenerateList ElmTypeExpr (F.Foreign ElmTypeExpr api))
  => ElmOptions
  -> Proxy api
  -> [String]
generateElmForAPIWith opts =
  nub . concatMap (generateElmForRequest opts) . getEndpoints


generateElmForRequest :: ElmOptions -> F.Req ElmTypeExpr -> [String]
generateElmForRequest opts request =
  typeSignatureDefs
  ++ bodyEncoderDefs
  ++ responseDecoderDefs
  ++ [funcDef]
  where
    funcDef =
      (intercalate "\n" . filter (not . null))
        ([ fnName ++ " : " ++ typeSignature
         , fnNameArgs ++ " ="
         , "  let"
         ]
         ++ letParams
         ++ letRequest
         ++ [ "  in" ]
         ++ httpRequest
         )

    fnName =
      T.unpack (F.camelCase (request ^. F.reqFuncName))

    (typeSignature, typeSignatureDefs) =
      mkTypeSignature opts request

    fnNameArgs =
      unwords (fnName : mkArgsList request)

    letParams =
      mkLetParams "    " opts request

    (letRequest, bodyEncoderDefs) =
      mkLetRequest "    " opts request

    (httpRequest, responseDecoderDefs) =
      mkHttpRequest "    " opts request


mkTypeSignature
  :: ElmOptions
  -> F.Req ElmTypeExpr
  -> ( String
       -- The type signature
     , [String]
       -- Supporting type definitions
     )
mkTypeSignature opts request =
  (collect . catMaybes)
    [ urlCaptureTypes
    , queryTypes
    , bodyType
    , returnType
    ]
  where
    collect :: [(String, [String])] -> (String, [String])
    collect xs =
      ( intercalate " -> " (map fst xs)
      , concatMap snd xs
      )

    urlCaptureArgs :: [F.Arg ElmTypeExpr]
    urlCaptureArgs =
      [ F.captureArg cap
      | cap <- request ^. F.reqUrl . F.path
      , F.isCapture cap
      ]

    urlCaptureTypes :: Maybe (String, [String])
    urlCaptureTypes =
      if null urlCaptureArgs then
        Nothing
      else
        Just $ collect
          [ Elm.toElmTypeSourceDefsWith
              (elmExportOptions opts)
              (arg ^. F.argType)
          | arg <- urlCaptureArgs
          ]

    queryArgToElmType :: F.QueryArg ElmTypeExpr -> (String, [String])
    queryArgToElmType arg =
      let
        (eType, eTypeDefs) =
          Elm.toElmTypeSourceDefsWith
            (elmExportOptions opts)
            (arg ^. F.queryArgName . F.argType)
      in
        ( case arg ^. F.queryArgType of
            F.Normal ->
              "Maybe (" ++ eType ++ ")"
            _ ->
              eType
        , eTypeDefs
        )

    queryTypes :: Maybe (String, [String])
    queryTypes =
      if null (request ^. F.reqUrl . F.queryStr) then
        Nothing
      else
        Just . collect $
          map queryArgToElmType (request ^. F.reqUrl . F.queryStr)

    bodyType :: Maybe (String, [String])
    bodyType =
      fmap
        (Elm.toElmTypeSourceDefsWith (elmExportOptions opts))
        (request ^. F.reqBody)

    mkReturnType :: ElmTypeExpr -> (String, [String])
    mkReturnType elmTypeExpr =
      first
        (\eType -> "Task.Task Http.Error (" ++ eType ++ ")")
        (Elm.toElmTypeSourceDefsWith (elmExportOptions opts) elmTypeExpr)

    returnType :: Maybe (String, [String])
    returnType =
      fmap mkReturnType (request ^. F.reqReturnType)


mkArgsList
  :: F.Req ElmTypeExpr
  -> [String]
mkArgsList request =
  -- URL Captures
  [ T.unpack . F.unPathSegment $ F.captureArg segment ^. F.argName
  | segment <- request ^. F.reqUrl . F.path
  , F.isCapture segment
  ]
  ++
  -- Query params
  [ T.unpack . F.unPathSegment $ arg ^. F.queryArgName . F.argName
  | arg <- request ^. F.reqUrl . F.queryStr
  ]
  ++
  -- Request body
  catMaybes
    [ fmap (const "body") (request ^. F.reqBody)
    ]


mkUrl
  :: ElmOptions
  -> [F.Segment ElmTypeExpr]
  -> String
mkUrl opts segments =
  (intercalate newLine . catMaybes)
    [ nullOr (urlPrefix opts) $
        "\"" ++ urlPrefix opts ++ "\""
    , nullOr segments $
        "\"/\" ++ "
        ++ intercalate (newLine ++ "\"/\" ++ ")
              (map segmentToStr segments)
    ]
  where
    newLine =
      "\n          ++ "

    nullOr t x =
      if null t then
         Nothing
      else
        Just x

    segmentToStr :: F.Segment ElmTypeExpr -> String
    segmentToStr s =
      case F.unSegment s of
        F.Static path ->
          "\"" ++ T.unpack (F.unPathSegment path) ++ "\""
        F.Cap arg ->
          let
            -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
            toStringSrc =
              if isElmStringType opts (arg ^. F.argType) then
                ""
              else
                " |> toString"
          in
            "(" ++ T.unpack (F.unPathSegment (arg ^. F.argName)) ++ toStringSrc ++ " |> Http.uriEncode)"


mkLetParams
  :: String
  -> ElmOptions
  -> F.Req ElmTypeExpr
  -> [String]
mkLetParams indent opts request =
  if null (request ^. F.reqUrl . F.queryStr) then
    []
  else
    map (indent ++)
        [ "params ="
        , "  List.filter (not << String.isEmpty)"
        , "    [ " ++ intercalate ("\n" ++ indent ++ "    , ") params
        , "    ]"
        ]
  where
    params :: [String]
    params = map paramToStr (request ^. F.reqUrl . F.queryStr)

    paramToStr :: F.QueryArg ElmTypeExpr -> String
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
            intercalate newLine
              [ name
              , "  |> Maybe.map (" ++ toStringSrc ++ "Http.uriEncode >> (++) \"" ++ name ++ "=\")"
              , "  |> Maybe.withDefault \"\""
              ]

        F.Flag ->
          intercalate newLine
            ["if " ++ name ++ " then"
            , "  \"" ++ name ++ "=\""
            , "else"
            , "  \"\""
            ]

        F.List ->
          intercalate newLine
            [ name
            , "  |> List.map (\\val -> \"" ++ name ++ "[]=\" ++ (val |> toString |> Http.uriEncode))"
            , "  |> String.join \"&\""
            ]
      where
        name =
          T.unpack . F.unPathSegment . view (F.queryArgName . F.argName) $ qarg
        newLine = "\n          "


mkLetRequest
  :: String
  -> ElmOptions
  -> F.Req ElmTypeExpr
  -> ( [String]
       -- The source lines for the Elm request value.
     , [String]
       -- Supporting definitions for the body JSON encoder.
     )
mkLetRequest indent opts request =
  ( map (indent ++)
      ([ "request ="
       , "  { verb ="
       , "      \"" ++ method ++ "\""
       , "  , headers ="
       , "      [(\"Content-Type\", \"application/json\")]"
       , "  , url ="
       , "      " ++ url
       ]
       ++ mkQueryParams "      " request ++
       [ "  , body ="
       , "      " ++ body
       , "  }"
       ]
      )
  , bodyEncoderDefs
  )
  where
    method =
       T.unpack (T.decodeUtf8 (request ^. F.reqMethod))

    url =
      mkUrl opts (request ^. F.reqUrl . F.path)

    (body, bodyEncoderDefs) =
      case request ^. F.reqBody of
        Nothing ->
          ( "Http.empty", [] )

        Just elmTypeExpr ->
          first
            (\encoderName ->
               "Http.string (Json.Encode.encode 0 (" ++ encoderName ++ " body))")
            (Elm.toElmEncoderSourceDefsWith (elmExportOptions opts) elmTypeExpr)


mkQueryParams
  :: String
  -> F.Req ElmTypeExpr
  -> [String]
mkQueryParams indent request =
  if null (request ^. F.reqUrl . F.queryStr) then
    []
  else
    map (indent ++)
    [ "++ if List.isEmpty params then"
    , "     \"\""
    , "   else"
    , "     \"?\" ++ String.join \"&\" params"
    ]


{-| If the return type has a decoder, construct the request using Http.fromJson.
Otherwise, construct an HTTP request that expects an empty response.
-}
mkHttpRequest
  :: String
  -> ElmOptions
  -> F.Req ElmTypeExpr
  -> ([String], [String])
mkHttpRequest indent opts request =
  ( map (indent ++) elmLines
  , responseDecoderDefs
  )
  where
    (elmLines, responseDecoderDefs) =
      case request ^. F.reqReturnType of
        Just elmTypeExpr | isEmptyType opts elmTypeExpr ->
          bimap
            emptyResponseRequest
            (++ [ emptyResponseHandlerSrc
                , handleResponseSrc
                , promoteErrorSrc
                ])
            (Elm.toElmTypeSourceDefsWith (elmExportOptions opts) elmTypeExpr)

        Just elmTypeExpr ->
          first jsonRequest (Elm.toElmDecoderSourceDefsWith (elmExportOptions opts) elmTypeExpr)

        Nothing ->
          error "mkHttpRequest: no reqReturnType?"

    jsonRequest decoder =
      [ "Http.fromJson"
      , "  " ++ decoder
      , "  (Http.send Http.defaultSettings request)"
      ]

    emptyResponseRequest elmType =
      [ "Task.mapError promoteError"
      , "  (Http.send Http.defaultSettings request)"
      , "    `Task.andThen`"
      , "      handleResponse (emptyResponseHandler " ++ elmType ++ ")"
      ]

    emptyResponseHandlerSrc =
      intercalate "\n"
        [ "emptyResponseHandler : a -> String -> Task.Task Http.Error a"
        , "emptyResponseHandler x str ="
        , "  if String.isEmpty str then"
        , "    Task.succeed x"
        , "  else"
        , "    Task.fail (Http.UnexpectedPayload str)"
        ]

    handleResponseSrc =
      intercalate "\n"
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
      intercalate "\n"
        [ "promoteError : Http.RawError -> Http.Error"
        , "promoteError rawError ="
        , "  case rawError of"
        , "    Http.RawTimeout -> Http.Timeout"
        , "    Http.RawNetworkError -> Http.NetworkError"
        ]


{- | Determines whether we construct an Elm function that expects an empty
response body.
-}
isEmptyType :: ElmOptions -> ElmTypeExpr -> Bool
isEmptyType opts elmTypeExpr =
  elmTypeExpr `elem` emptyResponseElmTypes opts


{- | Determines whether we call `toString` on URL captures and query params of
this type in Elm.
-}
isElmStringType :: ElmOptions -> ElmTypeExpr -> Bool
isElmStringType opts elmTypeExpr =
  elmTypeExpr `elem` stringElmTypes opts
