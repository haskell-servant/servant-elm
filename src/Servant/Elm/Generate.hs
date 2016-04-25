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
  }


{-|
The default options for generating Elm code.

[@urlPrefix@] (An empty string)
-}
defElmOptions :: ElmOptions
defElmOptions = ElmOptions
  { urlPrefix = ""
  , elmExportOptions = Elm.defaultOptions
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
  typeSigDefs
  ++ bodyEncoderDefs
  ++ responseDecoderDefs
  ++ [funcDef]
  where
    funcDef =
      (intercalate "\n" . filter (not . null))
        [ fnName ++ " : " ++ typeSig
        , fnNameArgs ++ " ="
        , "  let"
        , mkLetQueryParams "    " request
        , "    request ="
        , "      { verb ="
        , "          \"" ++ method ++ "\""
        , "      , headers ="
        , "          [(\"Content-Type\", \"application/json\")]"
        , "      , url ="
        , "          " ++ url
        , mkQueryParams "          " request
        , "      , body ="
        , "          " ++ body
        , "      }"
        , "  in"
        , httpRequest
        ]

    fnName =
      T.unpack (F.camelCase (request ^. F.reqFuncName))

    (typeSig, typeSigDefs) =
      typeSignature opts request

    fnNameArgs =
      unwords (fnName : args)

    args =
      [ T.unpack . F.unPathSegment $ F.captureArg segment ^. F.argName
      | segment <- request ^. F.reqUrl . F.path
      , F.isCapture segment
      ]
      ++
      [ T.unpack . F.unPathSegment $ arg ^. F.queryArgName . F.argName
      | arg <- request ^. F.reqUrl . F.queryStr
      ]
      ++
      catMaybes
        [ fmap (const "body") (request ^. F.reqBody)
        ]

    method =
       T.unpack (T.decodeUtf8 (request ^. F.reqMethod))

    url =
      mkUrl (urlPrefix opts) (request ^. F.reqUrl . F.path)

    (body, bodyEncoderDefs) =
      case request ^. F.reqBody of
        Nothing ->
          ( "Http.empty", [] )
        Just elmTypeExpr ->
          let
            (encoderName, encoderSourceDefs) =
              Elm.toElmEncoderSourceDefsWith (elmExportOptions opts) elmTypeExpr
          in
            ( "Http.string (Json.Encode.encode 0 (" ++ encoderName ++ " body))"
            , encoderSourceDefs
            )

    (httpRequest, responseDecoderDefs) =
      mkHttpRequest "    " opts request

typeSignature
  :: ElmOptions
  -> F.Req ElmTypeExpr
  -> ( String
       -- ^ The type signature
     , [String]
       -- ^ Supporting type definitions
     )
typeSignature opts request =
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

    urlCaptureArgs =
      [ F.captureArg cap
      | cap <- request ^. F.reqUrl . F.path
      , F.isCapture cap
      ]
    urlCaptureTypes =
      if null urlCaptureArgs then
        Nothing
      else
        Just $ collect
          [ Elm.toElmTypeSourceDefsWith (elmExportOptions opts) (arg ^. F.argType)
          | arg <- urlCaptureArgs
          ]

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

    queryTypes =
      if null (request ^. F.reqUrl . F.queryStr) then
        Nothing
      else
        Just . collect $
          map queryArgToElmType (request ^. F.reqUrl . F.queryStr)

    bodyType =
      fmap
        (Elm.toElmTypeSourceDefsWith (elmExportOptions opts))
        (request ^. F.reqBody)

    mkReturnType elmTypeExpr =
      first
        (\eType -> "Task.Task Http.Error (" ++ eType ++ ")")
        (Elm.toElmTypeSourceDefsWith (elmExportOptions opts) elmTypeExpr)

    returnType =
      fmap mkReturnType (request ^. F.reqReturnType)

mkUrl
  :: String
  -> [F.Segment ElmTypeExpr]
  -> String
mkUrl prefix segments =
  (intercalate newLine . catMaybes)
    [ nullOr prefix $
        "\"" ++ prefix ++ "\""
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

    segmentToStr s =
      case F.unSegment s of
        F.Static path ->
          "\"" ++ T.unpack (F.unPathSegment path) ++ "\""
        F.Cap arg ->
          let
            -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
            toStringSrc =
              if isElmStringType (arg ^. F.argType) then
                ""
              else
                " |> toString"
          in
            "(" ++ T.unpack (F.unPathSegment (arg ^. F.argName)) ++ toStringSrc ++ " |> Http.uriEncode)"

isElmStringType :: ElmTypeExpr -> Bool
isElmStringType elmTypeExpr =
  case elmTypeExpr of
    Elm.Primitive "String" ->
      True
    Elm.Product (Elm.Primitive "List") (Elm.Primitive "Char") ->
      True
    _ ->
      False

mkLetQueryParams
  :: String
  -> F.Req ElmTypeExpr
  -> String
mkLetQueryParams indent request =
  if null (request ^. F.reqUrl . F.queryStr) then
    ""
  else
    indent
    ++ intercalate ("\n" ++ indent)
        [ "params ="
        , "  List.filter (not << String.isEmpty)"
        , "    [ " ++ intercalate ("\n" ++ indent ++ "    , ") params
        , "    ]"
        ]
  where
    params = map paramToStr (request ^. F.reqUrl . F.queryStr)

    paramToStr qarg =
      case qarg ^. F.queryArgType of
        F.Normal ->
          let
            -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
            toStringSrc =
              if isElmStringType (qarg ^. F.queryArgName . F.argType) then
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
      where name = T.unpack . F.unPathSegment . view (F.queryArgName . F.argName) $ qarg
            newLine = "\n          "

mkQueryParams
  :: String
  -> F.Req ElmTypeExpr
  -> String
mkQueryParams indent request =
  if null (request ^. F.reqUrl . F.queryStr) then
    ""
  else
    indent
    ++ intercalate ("\n" ++ indent)
        [ "++ if List.isEmpty params then"
        , "     \"\""
        , "   else"
        , "     \"?\" ++ String.join \"&\" params"
        ]


{-| If the return type has a decoder, construct the request using Http.fromJson.
Otherwise, construct an HTTP request that expects an empty response.
-}
mkHttpRequest :: String -> ElmOptions -> F.Req ElmTypeExpr -> (String, [String])
mkHttpRequest indent opts request =
  ( indent ++ intercalate ("\n" ++ indent) elmLines
  , responseDecoderDefs
  )
  where
    isEmptyType elmTypeExpr =
      case elmTypeExpr of
        Elm.Primitive "()" -> True
        Elm.DataType "NoContent" _ -> True
        _ -> False

    (elmLines, responseDecoderDefs) =
      case request ^. F.reqReturnType of
        Just elmTypeExpr | isEmptyType elmTypeExpr ->
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
