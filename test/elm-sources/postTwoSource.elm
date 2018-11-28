module PostTwoSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)
import Json.Encode


postTwo : String -> Http.Request (Http.Response (Maybe (Int)))
postTwo body =
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
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString (nullable int) res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
