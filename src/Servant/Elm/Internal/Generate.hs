{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Servant.Elm.Internal.Generate where

import           Prelude                      hiding ((<$>))
import           Control.Lens                 (to, (^.))
import           Data.List                    (intercalate, intersperse, nub)
import           Data.Maybe                   (catMaybes)
import           Data.Proxy                   (Proxy(..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as L
import qualified Data.Text.Encoding           as T
import           Data.Text.IO                 as TIO

import           Elm.Json (jsonParserForType, jsonSerForType)
import qualified Elm.Module                   as Elm
import           Elm.TyRep (ETCon(..), EType(..), ETypeDef(..), toElmType)
import           Elm.TyRender (renderElm)
import           Elm.Versions (ElmVersion(Elm0p18))

import           Servant.Elm.Internal.Foreign (LangElm, getEndpoints)
import qualified Servant.Foreign              as F
import           System.Directory (createDirectoryIfMissing)
import           Text.PrettyPrint.Leijen.Text


toElmTypeRefWith :: ElmOptions -> EType -> Text
toElmTypeRefWith ElmOptions{..} = T.pack . renderElm . elmTypeAlterations

toElmDecoderRefWith :: ElmOptions -> EType -> Text
toElmDecoderRefWith ElmOptions{..} = T.pack . jsonParserForType . elmTypeAlterations

toElmEncoderRefWith :: ElmOptions -> EType -> Text
toElmEncoderRefWith ElmOptions{..} = T.pack . jsonSerForType . elmTypeAlterations

{-|
Options to configure how code is generated.
-}
data ElmOptions = ElmOptions
  { {- | The protocol, host and any path prefix to be used as the base for all
    requests.

    Example: @Static "https://mydomain.com/api/v1"@

    When @Dynamic@, the generated Elm functions take the base URL as the first
    argument.
    -}
    urlPrefix             :: UrlPrefix
  , elmTypeAlterations        :: (EType -> EType)
    -- ^ Alterations to perform on ETypes before code generation.
  , elmAlterations        :: (ETypeDef -> ETypeDef)
    -- ^ Alterations to perform on ETypeDefs before code generation.
  , emptyResponseElmTypes :: [EType]
    -- ^ Types that represent an empty Http response.
  , stringElmTypes        :: [EType]
    -- ^ Types that represent a String.
  }


data UrlPrefix
  = Static T.Text
  | Dynamic

type Namespace = [String]

{-|
Default options for generating Elm code.

The default options are:

> { urlPrefix =
>     Static ""
> , elmAlterations =
>     Elm.defaultTypeAlterations
> , emptyResponseElmTypes =
>     [ getType (Proxy :: Proxy ()) ]
> , stringElmTypes =
>     [ getType (Proxy :: Proxy String)
>     , getType (Proxy :: Proxy T.Text) ]
> }
-}
defElmOptions :: ElmOptions
defElmOptions = ElmOptions
  { urlPrefix = Static ""
  , elmTypeAlterations = Elm.defaultTypeAlterations
  , elmAlterations = Elm.defaultAlterations
  , emptyResponseElmTypes =
      [ toElmType (Proxy :: Proxy ())
      ]
  , stringElmTypes =
      [ toElmType (Proxy :: Proxy String)
      , toElmType (Proxy :: Proxy T.Text)
      ]
  }


{-|
Default imports required by generated Elm code.

You probably want to include this at the top of your generated Elm module.

The default required imports are:

> import Json.Decode
> import Json.Encode exposing (Value)
> -- The following module comes from bartavelle/json-helpers
> import Json.Helpers exposing (..)
> import Dict exposing (Dict)
> import Set
> import Http
> import String
> import Url.Builder
-}
defElmImports :: Text
defElmImports =
  T.unlines
    [ "import Json.Decode"
    , "import Json.Encode exposing (Value)"
    , "-- The following module comes from bartavelle/json-helpers"
    , "import Json.Helpers exposing (..)"
    , "import Dict exposing (Dict)"
    , "import Set"
    , "import Http"
    , "import String"
    , "import Url.Builder"
    , ""
    , "maybeBoolToIntStr : Maybe Bool -> String"
    , "maybeBoolToIntStr mx ="
    , "  case mx of"
    , "    Nothing -> \"\""
    , "    Just True -> \"1\""
    , "    Just False -> \"0\""
    ]

{-|
Helper to generate a complete Elm module given a list of Elm type definitions
and an API.
-}
generateElmModuleWith ::
     ( F.HasForeign LangElm EType api
     , F.GenerateList EType (F.Foreign EType api)
     )
  => ElmOptions
  -> Namespace
  -> Text
  -> FilePath
  -> [Elm.DefineElm]
  -> Proxy api
  -> IO ()
generateElmModuleWith options namespace imports rootDir typeDefs api = do
  let out =
        T.unlines $
        [ T.pack $ Elm.moduleHeader Elm0p18 moduleName
        , ""
        , imports
        , T.pack $ Elm.makeModuleContentWithAlterations (elmAlterations options) typeDefs
        ] ++
        generateElmForAPIWith options api
      moduleName = intercalate "." namespace
      filePath = intercalate "/" $ rootDir:init namespace
      fileName = intercalate "/" $ filePath:[last namespace ++ ".elm"]
  createDirectoryIfMissing True filePath
  TIO.writeFile fileName out

{-|
Calls generateElmModuleWith with @defElmOptions@.
-}
generateElmModule ::
     ( F.HasForeign LangElm EType api
     , F.GenerateList EType (F.Foreign EType api)
     )
  => Namespace
  -> Text
  -> FilePath
  -> [Elm.DefineElm]
  -> Proxy api
  -> IO ()
generateElmModule namespace imports filePath typeDefs api =
  generateElmModuleWith defElmOptions namespace imports filePath typeDefs api

{-|
Generate Elm code for the API with default options.

Returns a list of Elm functions to query your Servant API from Elm.

You could spit these out to a file and call them from your Elm code, but you
would be better off creating a 'Spec' with the result and using 'specsToDir',
which handles the module name for you.
-}
generateElmForAPI
  :: ( F.HasForeign LangElm EType api
     , F.GenerateList EType (F.Foreign EType api))
  => Proxy api
  -> [Text]
generateElmForAPI =
  generateElmForAPIWith defElmOptions


{-|
Generate Elm code for the API with custom options.
-}
generateElmForAPIWith
  :: ( F.HasForeign LangElm EType api
     , F.GenerateList EType (F.Foreign EType api))
  => ElmOptions
  -> Proxy api
  -> [Text]
generateElmForAPIWith opts = intersperse "" .
  nub . map docToText . map (generateElmForRequest opts) . getEndpoints

i :: Int
i = 4

{-|
Generate an Elm function for one endpoint.
-}
generateElmForRequest :: ElmOptions -> F.Req EType -> Doc
generateElmForRequest opts request =
  funcDef
  where
    funcDef =
      vsep
        [ fnName <+> ":" <+> typeSignature
        , fnName <+> args <+> equals
        , case letParams of
            Just params ->
              indent i
              (vsep ["let"
                    , indent i params
                    , "in"
                    , indent i elmRequest
                    ])
            Nothing ->
              indent i elmRequest
        ]

    fnName =
      request ^. F.reqFuncName . to (replace . F.camelCase) . to stext

    replace = T.replace "-" "" . T.replace "." ""

    typeSignature =
      mkTypeSignature opts request

    args =
      mkArgs opts request

    letParams =
      mkLetParams opts request

    elmRequest =
      mkRequest opts request


mkTypeSignature :: ElmOptions -> F.Req EType -> Doc
mkTypeSignature opts request =
  (hsep . punctuate " ->" . concat)
    [ catMaybes [urlPrefixType]
    , headerTypes
    , urlCaptureTypes
    , queryTypes
    , catMaybes [bodyType, toMsgType, returnType]
    ]
  where
    urlPrefixType :: Maybe Doc
    urlPrefixType =
        case (urlPrefix opts) of
          Dynamic -> Just "String"
          Static _ -> Nothing

    elmTypeRef :: EType -> Doc
    elmTypeRef eType =
      stext (toElmTypeRefWith opts eType)

    headerTypes :: [Doc]
    headerTypes =
      [ header ^. F.headerArg . F.argType . to elmTypeRef
      | header <- request ^. F.reqHeaders
      , isNotCookie header
      ]

    urlCaptureTypes :: [Doc]
    urlCaptureTypes =
        [ F.captureArg capture ^. F.argType . to elmTypeRef
        | capture <- request ^. F.reqUrl . F.path
        , F.isCapture capture
        ]

    queryTypes :: [Doc]
    queryTypes =
      [ arg ^. F.queryArgName . F.argType . to elmTypeRef
      | arg <- request ^. F.reqUrl . F.queryStr
      ]

    bodyType :: Maybe Doc
    bodyType =
        fmap elmTypeRef $ request ^. F.reqBody

    toMsgType :: Maybe Doc
    toMsgType = do
      result <- fmap elmTypeRef $ request ^. F.reqReturnType
      Just ("(Result Http.Error " <+> parens result <+> " -> msg)")

    returnType :: Maybe Doc
    returnType = do
      pure ("Cmd msg")


elmHeaderArg :: F.HeaderArg EType -> Doc
elmHeaderArg header =
  "header_" <>
  header ^. F.headerArg . F.argName . to (stext . T.replace "-" "_" . F.unPathSegment)


elmCaptureArg :: F.Segment EType -> Doc
elmCaptureArg segment =
  "capture_" <>
  F.captureArg segment ^. F.argName . to (stext . replace . F.unPathSegment)
  where
    replace = T.replace "-" "_"


elmQueryArg :: F.QueryArg EType -> Doc
elmQueryArg arg =
  "query_" <>
  arg ^. F.queryArgName . F.argName . to (stext . replace . F.unPathSegment)
  where
    replace = T.replace "-" "_"


elmBodyArg :: Doc
elmBodyArg =
  "body"


isNotCookie :: F.HeaderArg f -> Bool
isNotCookie header =
   header
     ^. F.headerArg
      . F.argName
      . to ((/= "cookie") . T.toLower . F.unPathSegment)


mkArgs
  :: ElmOptions
  -> F.Req EType
  -> Doc
mkArgs opts request =
  (hsep . concat) $
    [ -- Dynamic url prefix
      case urlPrefix opts of
        Dynamic -> ["urlBase"]
        Static _ -> []
    , -- Headers
      [ elmHeaderArg header
      | header <- request ^. F.reqHeaders
      , isNotCookie header
      ]
    , -- URL Captures
      [ elmCaptureArg segment
      | segment <- request ^. F.reqUrl . F.path
      , F.isCapture segment
      ]
    , -- Query params
      [ elmQueryArg arg
      | arg <- request ^. F.reqUrl . F.queryStr
      ]
    , -- Request body
      maybe [] (const [elmBodyArg]) (request ^. F.reqBody)
    , pure "toMsg"
    ]


mkLetParams :: ElmOptions -> F.Req EType -> Maybe Doc
mkLetParams opts request =
    Just $ "params =" <$>
           indent i ("List.filterMap identity" <$>
                      parens ("List.concat" <$>
                              indent i (elmList params)))
  where
    params :: [Doc]
    params = map paramToDoc (request ^. F.reqUrl . F.queryStr)

    paramToDoc :: F.QueryArg EType -> Doc
    paramToDoc qarg =
      -- something wrong with indentation here...
      case qarg ^. F.queryArgType of
        F.Normal ->
          let
            argType = qarg ^. F.queryArgName . F.argType
            wrapped = isElmMaybeType argType
            -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
            toStringSrc =
              if isElmStringType opts argType || isElmMaybeStringType opts argType then
                ""
              else
                "String.fromInt >> "
          in
              "[" <+> (if wrapped then elmName else "Just" <+> elmName) <> line <>
                (indent 4 ("|> Maybe.map" <+> parens (toStringSrc <> "Url.Builder.string" <+> dquotes name)))
                <+> "]"
              -- (if wrapped then name else "Just" <+> name) <$>
              -- indent 4 ("|> Maybe.map" <+> parens (toStringSrc <> "Http.encodeUri >> (++)" <+> dquotes (elmName <> equals)) <$>
              --           "|> Maybe.withDefault" <+> dquotes empty)

        F.Flag ->
            "[" <+>
            ("if" <+> elmName <+> "then" <$>
            indent 4 ("Just" <+> parens ("Url.Builder.string" <+> dquotes name <+> dquotes empty)) <$>
            indent 2 "else" <$>
            indent 4 "Nothing")
            <+> "]"

        F.List ->
            let
              argType = qarg ^. F.queryArgName . F.argType
              convertedVal =
                if isElmListOfMaybeBoolType argType then
                  parens ("maybeBoolToIntStr" <+> "val")
                else
                  "val"
            in
            elmName <$>
            indent 4 ("|> List.map"
                      <+> parens (backslash <> "val ->" <+> "Just"
                                  <+> parens ("Url.Builder.string"
                                              <+> dquotes (name <> "[]")
                                              <+> convertedVal)))
      where
        elmName = elmQueryArg qarg
        name = qarg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


mkRequest :: ElmOptions -> F.Req EType -> Doc
mkRequest opts request =
  "Http.request" <$>
  indent i
    (elmRecord
       [ "method =" <$>
         indent i (dquotes method)
       , "headers =" <$>
         indent i
           (elmListOfMaybes headers)
       , "url =" <$>
         indent i url
       , "body =" <$>
         indent i body
       , "expect =" <$>
         indent i expect
       , "timeout =" <$>
         indent i "Nothing"
       , "tracker =" <$>
         indent i "Nothing"
       ])
  where
    method =
       request ^. F.reqMethod . to (stext . T.decodeUtf8)

    mkHeader header =
      let headerName = header ^. F.headerArg . F.argName . to (stext . F.unPathSegment)
          headerArgName = elmHeaderArg header
          argType = header ^. F.headerArg . F.argType
          wrapped = isElmMaybeType argType
          toStringSrc =
            if isElmMaybeStringType opts argType || isElmStringType opts argType then
              mempty
            else
              " << String.fromInt"
      in
        "Maybe.map" <+> parens (("Http.header" <+> dquotes headerName <> toStringSrc))
        <+>
        (if wrapped then headerArgName else parens ("Just" <+> headerArgName))

    headers =
      [ mkHeader header
      | header <- request ^. F.reqHeaders
      , isNotCookie header
      ]

    url =
      mkUrl opts (request ^. F.reqUrl . F.path)
       <> mkQueryParams request

    body =
      case request ^. F.reqBody of
        Nothing ->
          "Http.emptyBody"

        Just elmTypeExpr ->
          let
            encoderName =
              toElmEncoderRefWith opts elmTypeExpr
          in
            "Http.jsonBody" <+> parens (stext encoderName <+> elmBodyArg)

    expect =
      case request ^. F.reqReturnType of
        Just elmTypeExpr
          | isEmptyType opts elmTypeExpr
            -- let elmConstructor = T.pack (renderElm elmTypeExpr)
           ->
            "Http.expectString " <> line <+> indent i "(\\x -> case x of" <> line <+>
            indent i "Err e -> toMsg (Err e)" <> line <+>
            indent i "Ok _ -> toMsg (Ok ()))"
        Just elmTypeExpr ->
          "Http.expectJson toMsg" <+> renderDecoderName elmTypeExpr
        Nothing -> error "mkHttpRequest: no reqReturnType?"
      -- case request ^. F.reqReturnType of
      --   Just elmTypeExpr | isEmptyType opts elmTypeExpr ->
      --     let elmConstructor =
      --           toElmTypeRefWith opts elmTypeExpr
      --     in
      --       "Http.expectStringResponse" <$>
      --       indent i (parens (backslash <> " rsp " <+> "->" <$>
      --                         indent i ("if String.isEmpty rsp.body then" <$>
      --                                   indent i "Ok" <+> stext elmConstructor <$>
      --                                   "else" <$>
      --                                   indent i ("Err" <+> dquotes "Expected the response body to be empty")) <> line))


      --   Just elmTypeExpr ->
      --     "Http.expectJson <|" <+> stext (toElmDecoderRefWith opts elmTypeExpr)

      --   Nothing ->
      --     error "mkHttpRequest: no reqReturnType?"

renderDecoderName :: EType -> Doc
renderDecoderName elmTypeExpr =
  case elmTypeExpr of
    ETyApp (ETyCon (ETCon "List")) t ->
      parens ("Json.Decode.list " <> parens (renderDecoderName t))
    ETyApp (ETyCon (ETCon "Maybe")) t ->
      parens ("Json.Decode.maybe " <> parens (renderDecoderName t))
    ETyCon (ETCon "Int") -> "Json.Decode.int"
    ETyCon (ETCon "String") -> "Json.Decode.string"
    _ -> ("jsonDec" <> stext (T.pack (renderElm elmTypeExpr)))


mkUrl :: ElmOptions -> [F.Segment EType] -> Doc
mkUrl opts segments =
  urlBuilder <$>
    (indent i . elmList)
    ( map segmentToDoc segments)
  -- ( case urlPrefix opts of
  --     Dynamic -> "urlBase"
  --     Static url -> dquotes (stext url)
  --   : map segmentToDoc segments)
  where
    urlBuilder :: Doc
    urlBuilder = case urlPrefix opts of
      Dynamic -> "Url.Builder.absolute" :: Doc
      Static url -> "Url.Builder.crossOrigin" <+> dquotes (stext url)

    segmentToDoc :: F.Segment EType -> Doc
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
                " |> String.fromInt"
          in
            (elmCaptureArg s) <> toStringSrc


mkQueryParams
  :: F.Req EType
  -> Doc
mkQueryParams _request =
  -- if null (request ^. F.reqUrl . F.queryStr) then
  --   empty
  -- else
    line <> indent 4 (align "params")


{- | Determines whether we construct an Elm function that expects an empty
response body.
-}
isEmptyType :: ElmOptions -> EType -> Bool
isEmptyType opts elmTypeExpr =
  elmTypeExpr `elem` emptyResponseElmTypes opts


{- | Determines whether we call `toString` on URL captures and query params of
this type in Elm.
-}
isElmStringType :: ElmOptions -> EType -> Bool
isElmStringType opts elmTypeExpr =
  elmTypeExpr `elem` stringElmTypes opts

{- | Determines whether a type is 'Maybe a' where 'a' is something akin to a 'String'.
-}
isElmMaybeStringType :: ElmOptions -> EType -> Bool
isElmMaybeStringType opts (ETyApp (ETyCon (ETCon "Maybe")) elmTypeExpr) = elmTypeExpr `elem` stringElmTypes opts
isElmMaybeStringType _ _ = False

isElmMaybeType :: EType -> Bool
isElmMaybeType (ETyApp (ETyCon (ETCon "Maybe")) _) = True
isElmMaybeType _ = False

isElmListOfMaybeBoolType :: EType -> Bool
isElmListOfMaybeBoolType t =
  case t of
    (ETyApp (ETyCon (ETCon "List")) (ETyApp (ETyCon (ETCon "Maybe")) (ETyCon (ETCon "Bool")))) -> True
    _ -> False

-- Doc helpers


docToText :: Doc -> Text
docToText =
  L.toStrict . displayT . renderPretty 0.4 100

stext :: Text -> Doc
stext = text . L.fromStrict

elmRecord :: [Doc] -> Doc
elmRecord = encloseSep (lbrace <> space) (line <> rbrace) (comma <> space)

elmList :: [Doc] -> Doc
elmList [] = lbracket <> rbracket
elmList ds = lbracket <+> hsep (punctuate (line <> comma) ds) <$> rbracket

elmListOfMaybes :: [Doc] -> Doc
elmListOfMaybes [] = lbracket <> rbracket
elmListOfMaybes ds = "List.filterMap identity" <$> indent 4 (elmList ds)
