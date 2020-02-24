module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import SimulatedEffect.Http
import ProgramTest
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

getIp : (Result (Maybe (Http.Metadata, String), Http.Error) (Response) -> msg) -> Cmd msg
getIp toMsg =
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
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeResponse body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getIpSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (Response) -> msg) -> ProgramTest.SimulatedEffect msg
getIpSimulated toMsg =
    SimulatedEffect.Http.request
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
            SimulatedEffect.Http.emptyBody
        , expect =
            SimulatedEffect.Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeResponse body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

getStatus204 : (Result (Maybe (Http.Metadata, String), Http.Error) (NoContent) -> msg) -> Cmd msg
getStatus204 toMsg =
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
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            if String.isEmpty body_ then
                                Ok (NoContent)
                            else
                                Err (Just (metadata, body_), Http.BadBody <| "Expected the response body to be empty, but it was '" ++ body_ ++ "'.")
                            )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getStatus204Simulated : (Result (Maybe (Http.Metadata, String), Http.Error) (NoContent) -> msg) -> ProgramTest.SimulatedEffect msg
getStatus204Simulated toMsg =
    SimulatedEffect.Http.request
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
            SimulatedEffect.Http.emptyBody
        , expect =
            SimulatedEffect.Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            if String.isEmpty body_ then
                                Ok (NoContent)
                            else
                                Err (Just (metadata, body_), Http.BadBody <| "Expected the response body to be empty, but it was '" ++ body_ ++ "'.")
                            )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

postPost : (Result (Maybe (Http.Metadata, String), Http.Error) (ResponseWithJson) -> msg) -> MessageBody -> Cmd msg
postPost toMsg body =
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
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeResponseWithJson body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postPostSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (ResponseWithJson) -> msg) -> MessageBody -> ProgramTest.SimulatedEffect msg
postPostSimulated toMsg body =
    SimulatedEffect.Http.request
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
            SimulatedEffect.Http.jsonBody (encodeMessageBody body)
        , expect =
            SimulatedEffect.Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeResponseWithJson body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

getGet : (Result (Maybe (Http.Metadata, String), Http.Error) (ResponseWithArgs) -> msg) -> Maybe (String) -> Cmd msg
getGet toMsg query_q =
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
                Http.expectStringResponse toMsg
                    (\res ->
                        case res of
                            Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                            Http.Timeout_ -> Err (Nothing, Http.Timeout)
                            Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                            Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                            Http.GoodStatus_ metadata body_ ->
                                (decodeString decodeResponseWithArgs body_)
                                    |> Result.mapError Json.Decode.errorToString
                                    |> Result.mapError Http.BadBody
                                    |> Result.mapError (Tuple.pair (Just (metadata, body_))))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }


getGetSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (ResponseWithArgs) -> msg) -> Maybe (String) -> ProgramTest.SimulatedEffect msg
getGetSimulated toMsg query_q =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_q
                    |> Maybe.map (Url.percentEncode >> (++) "q=")
                    |> Maybe.withDefault ""
                ]
    in
        SimulatedEffect.Http.request
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
                SimulatedEffect.Http.emptyBody
            , expect =
                SimulatedEffect.Http.expectStringResponse toMsg
                    (\res ->
                        case res of
                            Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                            Http.Timeout_ -> Err (Nothing, Http.Timeout)
                            Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                            Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                            Http.GoodStatus_ metadata body_ ->
                                (decodeString decodeResponseWithArgs body_)
                                    |> Result.mapError Json.Decode.errorToString
                                    |> Result.mapError Http.BadBody
                                    |> Result.mapError (Tuple.pair (Just (metadata, body_))))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getByPath : (Result (Maybe (Http.Metadata, String), Http.Error) (Response) -> msg) -> String -> Cmd msg
getByPath toMsg capture_path =
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
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeResponse body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getByPathSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (Response) -> msg) -> String -> ProgramTest.SimulatedEffect msg
getByPathSimulated toMsg capture_path =
    SimulatedEffect.Http.request
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
            SimulatedEffect.Http.emptyBody
        , expect =
            SimulatedEffect.Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeResponse body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }