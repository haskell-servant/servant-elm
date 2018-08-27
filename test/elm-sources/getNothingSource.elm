module GetNothingSource exposing (..)

import Http


getNothing : Http.Request (NoContent)
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
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }
