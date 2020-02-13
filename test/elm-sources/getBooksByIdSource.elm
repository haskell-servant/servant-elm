module GetBooksByIdSource exposing (..)

import String.Conversions as String
import Http
import SimulatedEffect.Http
import Url


getBooksById : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> Int -> Cmd msg
getBooksById toMsg capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_id |> String.fromInt |> Url.percentEncode
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
                            (decodeString decodeBook body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getBooksByIdSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> Int -> SimulatedEffect.Http.SimulatedEffect msg
getBooksByIdSimulated toMsg capture_id =
    SimulatedEffect.Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_id |> String.fromInt |> Url.percentEncode
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
                            (decodeString decodeBook body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
