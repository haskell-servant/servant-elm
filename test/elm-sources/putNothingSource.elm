module PutNothingSource exposing (..)

import String.Conversions as String
import Http


putNothing : Http.Request (Http.Response (()))
putNothing =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "nothing"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok { url = res.url, status = res.status, headers = res.headers, body = () }
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }
