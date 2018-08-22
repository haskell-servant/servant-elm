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
                (\response ->
                    Result.map
                        (\body -> { response | body = body })
                        (decodeString (nullable int) response.body))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
