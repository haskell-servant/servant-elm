module Servant.Elm.Generate where

import           Data.Char           (toLower)
import           Data.List           (intercalate, nub)
import           Data.Maybe          (catMaybes)
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


-- TODO: headers, content type?, url encoders?
generateElmForRequest :: ElmOptions -> Request -> [String]
generateElmForRequest opts request = typeDefs request ++ decoderDefs request ++ encoderDefs request ++ [func]
  where func = funcName ++ " : " ++ (typeSignature . reverse . fnSignature) request ++ "\n"
                  ++ funcNameArgs ++ " =\n"
                  ++ "  let request =\n"
                  ++ "        { verb = \"" ++ httpMethod request ++ "\"\n"
                  ++ "        , headers = [(\"Content-Type\", \"application/json\")]\n"
                  ++ "        , url = " ++ url ++ "\n"
                  ++ "        , body = " ++ body ++ "\n"
                  ++ "        }\n"
                  ++ "  in  Http.fromJson\n"
                  ++ "        " ++ decoder request ++ "\n"
                  ++ "        (Http.send Http.defaultSettings request)"
        funcName = (T.unpack . camelCase . map T.pack . (:) (map toLower (httpMethod request)) . reverse) (fnName request)
        typeSignature [x] = "Task.Task Http.Error (" ++ x ++ ")"
        typeSignature (x:xs) = x ++ " -> " ++ typeSignature xs
        typeSignature [] = ""
        funcNameArgs = unwords (funcName : args)
        url = buildUrl (urlPrefix opts) segments params
        args = reverse (argNames request)
        segments = (reverse . urlSegments) request
        params = (reverse . urlQueryStr) request
        body = case bodyEncoder request of
                 Just encoder -> "Http.string (JS.encode 0 (" ++ encoder ++ " body))"
                 Nothing -> "Http.empty"


buildUrl :: String -> [Segment] -> [QueryArg] -> String
buildUrl prefix segments params =
  (intercalate newLine . catMaybes)
    [ nullOr prefix $
        "\"" ++ prefix ++ "\""
    , nullOr segments $
        "\"/\" ++ "
        ++ intercalate (newLine ++ "\"/\" ++ ")
             (map segmentToStr segments)
    , nullOr params $
        "\"?\" ++ "
        ++ intercalate (newLine ++ "\"&\" ++ ")
             (map paramToStr params)
    ]
  where newLine = "\n             ++ "
        nullOr t x = if null t
                        then Nothing
                        else Just x


segmentToStr :: Segment -> String
segmentToStr (Segment (Static s)) = "\"" ++ T.unpack s ++ "\""
segmentToStr (Segment (Cap s))    = "(" ++ T.unpack s ++ " |> toString |> Http.uriEncode)"


paramToStr :: QueryArg -> String
paramToStr qarg =
  case _argType qarg of
    Normal -> "\"" ++ name ++ "=\" ++ (" ++ name ++ " |> toString |> Http.uriEncode)"
    Flag   -> "if " ++ name ++ " then \"" ++ name ++ "=\" else \"\""
    List   -> "String.join \"&\" (List.map (\\val -> \"" ++ name ++ "[]=\" ++ (val |> toString |> Http.uriEncode)) " ++ name ++ ")"
  where name = T.unpack (_argName qarg)
