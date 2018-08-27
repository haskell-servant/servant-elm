module PutNothingSource exposing (..)

import Http


putNothing : Http.Request (())
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
                        Ok ()
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }
