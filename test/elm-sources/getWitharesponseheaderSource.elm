module GetWithAResponseHeaderSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)


getWitharesponseheader : Http.Request (Http.Response (String))
getWitharesponseheader =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "with-a-response-header"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString string res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
