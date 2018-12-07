module PostTwoSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)
import Json.Encode


postTwo : (Result Http.Error (Maybe (Int)) -> msg) -> String -> Cmd msg
postTwo toMsg body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "two"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Http.BadUrl url)
                        Http.Timeout_ -> Err Http.Timeout
                        Http.NetworkError_ -> Err Http.NetworkError
                        Http.BadStatus_ metadata _ -> Err (Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString (nullable int) body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
