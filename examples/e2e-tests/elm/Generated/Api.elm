module Generated.Api exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String

type alias MessageBody  =
   { message: String
   }

jsonDecMessageBody : Json.Decode.Decoder ( MessageBody )
jsonDecMessageBody =
   (Json.Decode.string) >>= \pmessage ->
   Json.Decode.succeed {message = pmessage}

jsonEncMessageBody : MessageBody -> Value
jsonEncMessageBody  val =
   Json.Encode.string val.message


type alias QueryArgs  =
   { q: String
   }

jsonDecQueryArgs : Json.Decode.Decoder ( QueryArgs )
jsonDecQueryArgs =
   (Json.Decode.string) >>= \pq ->
   Json.Decode.succeed {q = pq}

jsonEncQueryArgs : QueryArgs -> Value
jsonEncQueryArgs  val =
   Json.Encode.string val.q


type alias Response  =
   { origin: String
   }

jsonDecResponse : Json.Decode.Decoder ( Response )
jsonDecResponse =
   (Json.Decode.string) >>= \porigin ->
   Json.Decode.succeed {origin = porigin}

jsonEncResponse : Response -> Value
jsonEncResponse  val =
   Json.Encode.string val.origin


type alias ResponseWithJson  =
   { json: MessageBody
   }

jsonDecResponseWithJson : Json.Decode.Decoder ( ResponseWithJson )
jsonDecResponseWithJson =
   (jsonDecMessageBody) >>= \pjson ->
   Json.Decode.succeed {json = pjson}

jsonEncResponseWithJson : ResponseWithJson -> Value
jsonEncResponseWithJson  val =
   jsonEncMessageBody val.json


type alias ResponseWithArgs  =
   { args: QueryArgs
   }

jsonDecResponseWithArgs : Json.Decode.Decoder ( ResponseWithArgs )
jsonDecResponseWithArgs =
   (jsonDecQueryArgs) >>= \pargs ->
   Json.Decode.succeed {args = pargs}

jsonEncResponseWithArgs : ResponseWithArgs -> Value
jsonEncResponseWithArgs  val =
   jsonEncQueryArgs val.args

getIp : Http.Request Response
getIp =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "https://httpbin.org"
                , "ip"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson <| jsonDecResponse
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getStatus204 : Http.Request NoContent
getStatus204 =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "https://httpbin.org"
                , "status"
                , "204"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postPost : MessageBody -> Http.Request ResponseWithJson
postPost body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "https://httpbin.org"
                , "post"
                ]
        , body =
            Http.jsonBody (jsonEncMessageBody body)
        , expect =
            Http.expectJson <| jsonDecResponseWithJson
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getGet : (Maybe String) -> Http.Request ResponseWithArgs
getGet query_q =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_q
                    |> Maybe.map (Http.encodeUri >> (++) "q=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                String.join "/"
                    [ "https://httpbin.org"
                    , "get"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| jsonDecResponseWithArgs
            , timeout =
                Nothing
            , withCredentials =
                False
            }

getByPath : String -> Http.Request Response
getByPath capture_path =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "https://httpbin.org"
                , capture_path |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson <| jsonDecResponse
        , timeout =
            Nothing
        , withCredentials =
            False
        }
