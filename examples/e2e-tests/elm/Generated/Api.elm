module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String.Conversions as String
import Url

type alias Response =
    { origin : String
    }

decodeResponse : Decoder Response
decodeResponse =
    succeed Response
        |> required "origin" string

type NoContent
    = NoContent

type alias MessageBody =
    { message : String
    }

encodeMessageBody : MessageBody -> Json.Encode.Value
encodeMessageBody x =
    Json.Encode.object
        [ ( "message", Json.Encode.string x.message )
        ]

decodeMessageBody : Decoder MessageBody
decodeMessageBody =
    succeed MessageBody
        |> required "message" string

type alias ResponseWithJson =
    { json : MessageBody
    }

decodeResponseWithJson : Decoder ResponseWithJson
decodeResponseWithJson =
    succeed ResponseWithJson
        |> required "json" decodeMessageBody

type alias QueryArgs =
    { q : String
    }

decodeQueryArgs : Decoder QueryArgs
decodeQueryArgs =
    succeed QueryArgs
        |> required "q" string

type alias ResponseWithArgs =
    { args : QueryArgs
    }

decodeResponseWithArgs : Decoder ResponseWithArgs
decodeResponseWithArgs =
    succeed ResponseWithArgs
        |> required "args" decodeQueryArgs

getIp : Http.Request (Http.Response (Response))
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
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString decodeResponse res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getStatus204 : Http.Request (Http.Response (NoContent))
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
                (\res ->
                    if String.isEmpty res.body then
                        Ok { url = res.url, status = res.status, headers = res.headers, body = NoContent }
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postPost : MessageBody -> Http.Request (Http.Response (ResponseWithJson))
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
            Http.jsonBody (encodeMessageBody body)
        , expect =
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString decodeResponseWithJson res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getGet : Maybe (String) -> Http.Request (Http.Response (ResponseWithArgs))
getGet query_q =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_q
                    |> Maybe.map (Url.percentEncode >> (++) "q=")
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
                Http.expectStringResponse
                    (\res ->
                        Result.mapError Json.Decode.errorToString
                            (Result.map
                                (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                                (decodeString decodeResponseWithArgs res.body)))
            , timeout =
                Nothing
            , withCredentials =
                False
            }

getByPath : String -> Http.Request (Http.Response (Response))
getByPath capture_path =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "https://httpbin.org"
                , capture_path |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString decodeResponse res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
