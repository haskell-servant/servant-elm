module GetOneSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)


getOne : Http.Request (Http.Response (Int))
getOne =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "one"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString int res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
