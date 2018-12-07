module GetOneWithDynamicUrlSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)


getOne : (Result Http.Error (Int) -> msg) -> String -> Cmd msg
getOne toMsg urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "one"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Http.BadUrl url)
                        Http.Timeout_ -> Err Http.Timeout
                        Http.NetworkError_ -> Err Http.NetworkError
                        Http.BadStatus_ metadata _ -> Err (Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString int body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
