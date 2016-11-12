{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Servant.Elm.Internal.Generate where

import           Prelude                      hiding ((<$>))
import           Control.Lens                 (to, (^.))
import           Data.List                    (nub)
import           Data.Maybe                   (catMaybes)
import           Data.Proxy                   (Proxy)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as L
import qualified Data.Text.Encoding           as T
import           Elm                          (ElmDatatype)
import qualified Elm
import           Servant.API                  (NoContent (..))
import           Servant.Elm.Internal.Foreign (LangElm, getEndpoints)
import           Servant.Elm.Internal.Orphans ()
import qualified Servant.Foreign              as F
import           Text.PrettyPrint.Leijen.Text


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
      , Elm.toElmType ()
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
  nub . map docToText . concatMap (generateElmForRequest opts) . getEndpoints

{-|
Generate an Elm function for one endpoint.

This function returns a list because the query function may require some
supporting definitions.
-}
generateElmForRequest :: ElmOptions -> F.Req ElmDatatype -> [Doc]
generateElmForRequest opts request =
  supportingFunctions ++ [funcDef]
  where
    funcDef =
      vsep
        [ fnName <+> ":" <+> typeSignature
        , fnName <+> args <+> equals
        , indent 2
            (vsep ["let"
                  , indent 2
                      ((maybe empty (<> line) letParams) <>
                       letRequest)
                  , "in"
                  , indent 2 httpRequest
                  ])
        ]

    fnName =
      request ^. F.reqFuncName . to F.camelCase . to stext

    typeSignature =
      mkTypeSignature opts request

    args =
      mkArgs request

    letParams =
      mkLetParams opts request

    letRequest =
      mkLetRequest opts request

    (httpRequest, supportingFunctions) =
      mkHttpRequest opts request


mkTypeSignature :: ElmOptions -> F.Req ElmDatatype -> Doc
mkTypeSignature opts request =
  (hsep . punctuate " ->" . concat)
    [ headerTypes
    , urlCaptureTypes
    , queryTypes
    , catMaybes [bodyType, returnType]
    ]
  where
    elmTypeRef :: ElmDatatype -> Doc
    elmTypeRef eType =
      stext (Elm.toElmTypeRefWith (elmExportOptions opts) eType)

    headerTypes :: [Doc]
    headerTypes =
      [ header ^. F.headerArg . F.argType . to elmTypeRef
      | header <- request ^. F.reqHeaders
      ]

    urlCaptureTypes :: [Doc]
    urlCaptureTypes =
        [ F.captureArg capture ^. F.argType . to elmTypeRef
        | capture <- request ^. F.reqUrl . F.path
        , F.isCapture capture
        ]

    queryTypes :: [Doc]
    queryTypes =
      [ arg ^. F.queryArgName . F.argType . to (elmTypeRef . wrapper)
      | arg <- request ^. F.reqUrl . F.queryStr
      , wrapper <- [
          case arg ^. F.queryArgType of
            F.Normal ->
              Elm.ElmPrimitive . Elm.EMaybe
            _ ->
              id
          ]
      ]

    bodyType :: Maybe Doc
    bodyType =
        fmap elmTypeRef $ request ^. F.reqBody

    returnType :: Maybe Doc
    returnType = do
      result <- fmap elmTypeRef $ request ^. F.reqReturnType
      pure ("Task.Task Http.Error" <+> parens result)


mkArgs
  :: F.Req ElmDatatype
  -> Doc
mkArgs request =
  (hsep . concat) $
    [ -- Headers
      [ header ^. F.headerArg . F.argName . to (stext . F.unPathSegment)
      | header <- request ^. F.reqHeaders
      ]
    , -- URL Captures
      [ F.captureArg segment ^. F.argName . to (stext . F.unPathSegment)
      | segment <- request ^. F.reqUrl . F.path
      , F.isCapture segment
      ]
    , -- Query params
      [ arg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)
      | arg <- request ^. F.reqUrl . F.queryStr
      ]
    , -- Request body
      maybe [] (const ["body"]) (request ^. F.reqBody)
    ]


mkLetParams :: ElmOptions -> F.Req ElmDatatype -> Maybe Doc
mkLetParams opts request =
  if null (request ^. F.reqUrl . F.queryStr) then
    Nothing
  else
    Just $ "params =" <$>
           indent 2 ("List.filter (not << String.isEmpty)" <$>
                      indent 2 (elmList params))
  where
    params :: [Doc]
    params = map paramToDoc (request ^. F.reqUrl . F.queryStr)

    paramToDoc :: F.QueryArg ElmDatatype -> Doc
    paramToDoc qarg =
      -- something wrong with indentation here...
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
              name <$>
              indent 4 ("|> Maybe.map" <+> parens (toStringSrc <> "Http.uriEncode >> (++)" <+> dquotes (name <> equals)) <$>
                        "|> Maybe.withDefault" <+> dquotes empty)

        F.Flag ->
            "if" <+> name <+> "then" <$>
            indent 4 (dquotes (name <> equals)) <$>
            indent 2 "else" <$>
            indent 4 (dquotes empty)

        F.List ->
            name <$>
            indent 4 ("|> List.map" <+> parens (backslash <> "val ->" <+> dquotes (name <> "[]=") <+> "++ (val |> toString |> Http.uriEncode)") <$>
                      "|> String.join" <+> dquotes "&")
      where
        name =
          qarg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


mkLetRequest :: ElmOptions -> F.Req ElmDatatype -> Doc
mkLetRequest opts request =
  "request =" <$>
  indent 2
    (elmRecord
       [ "verb =" <$>
         indent 4 (dquotes method)
       , "headers =" <$>
         indent 4
           (list (contentType : headers))
       , "url =" <$>
         indent 4 url
       , "body =" <$>
         indent 4 body
       ])
  where
    method =
       request ^. F.reqMethod . to (stext . T.decodeUtf8)

    contentType =
      parens (dquotes "Content-Type" <> comma <+> dquotes "application/json")

    headers =
        [parens (dquotes headerName <> comma <+>
                 (if isElmStringType opts (header ^. F.headerArg . F.argType) then
                     ""
                   else
                     "toString "
                  ) <> headerName)
        | header <- request ^. F.reqHeaders
        , headerName <- [header ^. F.headerArg . F.argName . to (stext . F.unPathSegment)]
        ]

    url =
      mkUrl opts (request ^. F.reqUrl . F.path)
       <> mkQueryParams request

    body =
      case request ^. F.reqBody of
        Nothing ->
          "Http.empty"

        Just elmTypeExpr ->
          let
            encoderName =
              Elm.toElmEncoderRefWith (elmExportOptions opts) elmTypeExpr
          in
            "Http.string" <+> parens ("Json.Encode.encode 0" <+> parens (stext encoderName <+> "body"))


mkUrl :: ElmOptions -> [F.Segment ElmDatatype] -> Doc
mkUrl opts segments =
  "String.join" <+> dquotes "/" <$>
  (indent 2 . elmList)
    ( dquotes (stext (urlPrefix opts))
    : map segmentToDoc segments)
  where

    segmentToDoc :: F.Segment ElmDatatype -> Doc
    segmentToDoc s =
      case F.unSegment s of
        F.Static path ->
          dquotes (stext (F.unPathSegment path))
        F.Cap arg ->
          let
            -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
            toStringSrc =
              if isElmStringType opts (arg ^. F.argType) then
                empty
              else
                " |> toString"
          in
            (arg ^. F.argName . to (stext . F.unPathSegment )) <> toStringSrc <> " |> Http.uriEncode"


mkQueryParams
  :: F.Req ElmDatatype
  -> Doc
mkQueryParams request =
  if null (request ^. F.reqUrl . F.queryStr) then
    empty
  else
    line <> "++" <+> align ("if List.isEmpty params then" <$>
                            indent 2 (dquotes empty) <$>
                            "else" <$>
                            indent 2 (dquotes "?" <+> "++ String.join" <+> dquotes "&" <+> "params"))


{-| If the return type has a decoder, construct the request using Http.fromJson.
Otherwise, construct an HTTP request that expects an empty response.
-}
mkHttpRequest
  :: ElmOptions
  -> F.Req ElmDatatype
  -> (Doc, [Doc])
mkHttpRequest opts request =
  ( elmRequest
  , supportingFunctions
  )
  where
    (elmRequest, supportingFunctions) =
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
      vsep
        [ "Http.fromJson"
        , "  " <> stext decoder
        , "  (Http.send Http.defaultSettings request)"
        ]

    emptyResponseRequest elmType =
      vsep
        [ "Task.mapError promoteError"
        , "  (Http.send Http.defaultSettings request)"
        , "    `Task.andThen`"
        , "      handleResponse (emptyResponseHandler " <> stext elmType <> ")"
        ]

    emptyResponseHandlerSrc =
      vsep
        [ "emptyResponseHandler : a -> String -> Task.Task Http.Error a"
        , "emptyResponseHandler x str ="
        , "  if String.isEmpty str then"
        , "    Task.succeed x"
        , "  else"
        , "    Task.fail (Http.UnexpectedPayload str)"
        ]

    handleResponseSrc =
      vsep
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
      vsep
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


-- Doc helpers


docToText :: Doc -> Text
docToText =
  L.toStrict . displayT . renderPretty 0.4 100

stext :: Text -> Doc
stext = text . L.fromStrict

elmRecord :: [Doc] -> Doc
elmRecord = encloseSep (lbrace <> space) (line <> rbrace) (comma <> space)

elmList :: [Doc] -> Doc
elmList ds = lbracket <+> hsep (punctuate (line <> comma) ds) <$> rbracket
