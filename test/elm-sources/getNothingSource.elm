module GetNothingSource exposing (..)

import String.Conversions as String
import Http


getNothing : Http.Request (Http.Response (NoContent))
getNothing =
    Http.request
        { method =
            "GET"
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
                        Ok { url = res.url, status = res.status, headers = res.headers, body = NoContent }
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }
