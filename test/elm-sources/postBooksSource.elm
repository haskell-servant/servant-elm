module PostBooksSource exposing (..)

import String.Conversions as String
import Http


postBooks : Book -> Http.Request (Http.Response (NoContent))
postBooks body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                ]
        , body =
            Http.jsonBody (encodeBook body)
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
