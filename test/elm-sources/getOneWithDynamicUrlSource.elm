module GetOneWithDynamicUrlSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)


getOne : (Result (Maybe Http.Metadata, Http.Error) (Int) -> msg) -> String -> Cmd msg
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
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata _ -> Err (Just metadata, Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString int body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just metadata)))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
