module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Response =
    { origin : String
    }

decodeResponse : Decoder Response
decodeResponse =
    decode Response
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
    decode MessageBody
        |> required "message" string

type alias ResponseWithJson =
    { json :MessageBody
    }

decodeResponseWithJson : Decoder ResponseWithJson
decodeResponseWithJson =
    decode ResponseWithJson
        |> required "json" decodeMessageBody

type alias QueryArgs =
    { q : String
    }

decodeQueryArgs : Decoder QueryArgs
decodeQueryArgs =
    decode QueryArgs
        |> required "q" string

type alias ResponseWithArgs =
    { args :QueryArgs
    }

decodeResponseWithArgs : Decoder ResponseWithArgs
decodeResponseWithArgs =
    decode ResponseWithArgs
        |> required "args" decodeQueryArgs

getIp : Http.Request (Response)
getIp =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , url =
            String.join "/"
                [ "https://httpbin.org"
                , "ip"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeResponse
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getStatus204 : Http.Request (NoContent)
getStatus204 =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
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

postPost : MessageBody -> Http.Request (ResponseWithJson)
postPost body =
    Http.request
        { method =
            "POST"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , url =
            String.join "/"
                [ "https://httpbin.org"
                , "post"
                ]
        , body =
            Http.jsonBody (encodeMessageBody body)
        , expect =
            Http.expectJson decodeResponseWithJson
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getGet : Maybe (String) -> Http.Request (ResponseWithArgs)
getGet q =
    let
        params =
            List.filter (not << String.isEmpty)
                [ q
                    |> Maybe.map (Http.encodeUri >> (++) "q=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                [ Http.header "Content-Type" "application/json"
                ]
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
                Http.expectJson decodeResponseWithArgs
            , timeout =
                Nothing
            , withCredentials =
                False
            }

getByPath : String -> Http.Request (Response)
getByPath path =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , url =
            String.join "/"
                [ "https://httpbin.org"
                , path |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeResponse
        , timeout =
            Nothing
        , withCredentials =
            False
        }